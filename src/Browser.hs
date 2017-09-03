{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}


import Floss.DB

import Text.Hamlet
import Text.Lucius
import Yesod hiding ((==.))

import Data.Text
import qualified Database.Persist as P
import qualified Database.Persist.Sqlite as P
import Database.Persist.TH
import Database.Esqueleto


import System.Environment
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger (runStderrLoggingT)
import Data.Maybe

data Browser = Browser ConnectionPool

instance Yesod Browser

mkYesod
  "Browser"
  [parseRoutes|
    /                          HomeR        GET
    /software/#String          SoftwareR    GET
    /softwarebyid/#Int   SoftwareIdR  GET
    /bylicenseid/#Int          ByLicenseIdR  GET
    /bylicense/#String          ByLicenseR  GET
    /bycodingid/#Int          ByCodingIdR  GET
    /bycoding/#String          ByCodingR  GET
|]

instance YesodPersist Browser where
    type YesodPersistBackend Browser = SqlBackend
    runDB action = do
        Browser pool <- getYesod
        runSqlPool action pool

-- Simple Header providing a Home-Link
header :: Widget
header = do
      toWidget
        [lucius|
            .header {
                text-align: center
            }
        |]
      toWidget
        [hamlet|
          <div class="header">
            <a href=@{HomeR}>Home
        |]

-- Query to get the list of all licenses
-- TODO: Cache results?
licenselist :: HandlerT Browser IO [Entity License]
licenselist = runDB 
           $ select $ distinct
           $ from $ \(pl `InnerJoin` l) -> do
                on $ l ^. LicenseId ==. pl ^. ProjectLicenseFkLicenseId
                limit 50
                orderBy [ asc (l ^. LicenseName) ]
                return l

---- TODO: Cache results?
codinglist :: HandlerT Browser IO [Entity Coding]
codinglist = runDB 
           $ select $ distinct
           $ from $ \(pc `InnerJoin` c) -> do
                on $ c ^. CodingId ==. pc ^. ProjectCodingFkCodingId
                limit 50
                orderBy [ asc (c ^. CodingName) ]
                return c
-- Chooser, to allow filtering for License, etc.
-- For now it works via Page-Redirect and the Recource-Handler do the work
-- The list of what options are available is currently given as an argument
-- TODO: Move to separate files
-- TODO: Add other filters
-- TODO: Preselect current value
chooser :: Widget
chooser = do
    ll <- handlerToWidget $ licenselist
    cl <- handlerToWidget $ codinglist
    toWidget
      [hamlet|
       <form action="#">
           <label> Lizenzen 
               <select name="license" id="licensechooser" onclick="chooselicense()">
                   <option>-- all --
                   $forall Entity licenseid license <- ll
                       <option>
                           $with wikidataid <- fromSqlKey licenseid
                               $maybe name <- (licenseName license)
                                   #{name}
           <label> Programiersprachen 
               <select name="coding" id="codingchooser" onclick="choosecoding()">
                   <option>-- all --
                   $forall Entity codingid coding <- cl
                       <option>
                           $with wikidataid <- fromSqlKey codingid
                               $maybe name <- (codingName coding)
                                   #{name}
      |]
    toWidget 
        [julius|
          function chooselicense() { 
            value = document.getElementById("licensechooser").value;
            if(value === "-- all --") {
                  window.location.href = "/";
               }
            else {
                  window.location.href = "/bylicense/" + value;
                 }
          }
          function choosecoding() { 
            value = document.getElementById("codingchooser").value;
            if(value === "-- all --") {
                  window.location.href = "/";
               }
            else {
                  window.location.href = "/bycoding/" + value;
                 }
          }
        |]

-- Compose Hamlet- and Lucius-Template of the software list
softwarelist :: (HandlerSite m ~ Browser, MonadWidget m) =>
     [Entity Project] -> m ()
softwarelist results = do
       toWidget $(whamletFile "./templates/softwarelist.hamlet")
       toWidget $(luciusFile "./templates/softwarelist.lucius")

-- TODO: remove duplicate code of runquery' or runquery
--          - Ether get rid of runquery' entirely or
--          - base runquery on runquery' or
--          - ...
runquery
  :: (BaseBackend (YesodPersistBackend site) ~ SqlBackend,
      YesodPersist site, IsPersistBackend (YesodPersistBackend site),
      PersistQueryRead (YesodPersistBackend site),
      PersistUniqueRead (YesodPersistBackend site)) =>
     Maybe String -> Maybe String -> HandlerT site IO [Entity Project]
runquery license coding = runDB
           $ select $ distinct
           $ from $ \(p `InnerJoin` pl `InnerJoin` l `InnerJoin` pc `InnerJoin` c) -> do
                on $ p ^. ProjectId ==. pl ^. ProjectLicenseFkProjectId
                on $ p ^. ProjectId ==. pc ^. ProjectCodingFkProjectId
                on $ l ^. LicenseId ==. pl ^. ProjectLicenseFkLicenseId
                on $ c ^. CodingId ==. pc ^. ProjectCodingFkCodingId
                case license of
                  Just license' -> where_ ( l ^. LicenseName ==. val (Just (pack license')))
                  Nothing -> return ()
                case coding of
                  Just coding' -> where_ ( c ^. CodingName ==. val (Just (pack coding')))
                  Nothing -> return ()
                limit 50
                return p

runquery'
  :: (BaseBackend (YesodPersistBackend site) ~ SqlBackend,
      YesodPersist site, IsPersistBackend (YesodPersistBackend site),
      PersistQueryRead (YesodPersistBackend site),
      PersistUniqueRead (YesodPersistBackend site)) =>
     Maybe Int -> Maybe Int -> HandlerT site IO [Entity Project]
runquery' license coding = runDB
           $ select $ distinct
           $ from $ \(p `InnerJoin` pl `InnerJoin` pc) -> do
                on $ p ^. ProjectId ==. pl ^. ProjectLicenseFkProjectId
                on $ p ^. ProjectId ==. pc ^. ProjectCodingFkProjectId
                case license of
                  Just license' -> where_ ( pl ^. ProjectLicenseFkLicenseId ==. val (qidtokey license') )
                  Nothing -> return ()
                case coding of
                  Just coding' -> where_ ( pc ^. ProjectCodingFkCodingId ==. val (qidtokey coding') )
                  Nothing -> return ()
                limit 50
                return p

---- Recourse handlers – mostly doing the same things -----
---- TODO: remove duplicate code                      -----

-- List all Software
getHomeR :: Handler Html
getHomeR = do
    results <- runDB $ P.selectList []  [P.LimitTo 50]
    defaultLayout $ do
       setTitle "Floss-Browser"
       softwarelist results

-- Show Details to one specified Software
getSoftwareR :: String -> Handler Html
getSoftwareR software = do
    results <- runDB $ P.selectList [ ProjectName P.==. (Just $ pack software) ]  [P.LimitTo 1]
    defaultLayout $ do
      setTitle $ toHtml $ "Flossbrowser: " ++ software
      toWidget $(whamletFile "./templates/software.hamlet")
      --toWidget $(luciusFile "./templates/software.lucius")


-- Show Details to one specified Software
getSoftwareIdR :: Int -> Handler Html
getSoftwareIdR qid = do
    results <- runDB
           $ select $ distinct
           $ from $ \p -> do
                where_ ( p ^. ProjectId ==. val (qidtokey qid) )
                limit 1
                return p
    let software = "Q" ++ (show qid)
    defaultLayout $ do
      setTitle $ toHtml $ "Flossbrowser: " ++ software
      toWidget $(whamletFile "./templates/software.hamlet")
      --toWidget $(luciusFile "./templates/software.lucius")

-- Get Software my Licence-ID
getByLicenseIdR :: Int -> Handler Html
getByLicenseIdR license = do
    results <- runquery' (Just license) Nothing
    defaultLayout $ do
      setTitle $ toHtml $ "Floss-Browser: Software licensed with license Q" ++ (show license)
      softwarelist results

-- Get Software by License-Name
getByLicenseR :: String -> Handler Html
getByLicenseR license = do
    results <- runquery (Just license) Nothing
    defaultLayout $ do
      setTitle $ toHtml $ "Floss-Browser: Software licensed with license " ++ license
      softwarelist results

-- Get Software my Coding-ID
getByCodingIdR :: Int -> Handler Html
getByCodingIdR coding = do
    results <- runquery' Nothing (Just coding)
    defaultLayout $ do
      setTitle $ toHtml $ "Floss-Browser: Software written in Q" ++ (show coding)
      softwarelist results

-- Get Software my Coding-Name
getByCodingR :: String -> Handler Html
getByCodingR coding = do
    results <- runquery Nothing (Just coding)
    defaultLayout $ do
      setTitle $ toHtml $ "Floss-Browser: Software written in " ++ coding
      softwarelist results


main :: IO ()
main = do
  t <- lookupEnv "PORT"
  let port = fromMaybe 3000 $ toint <$> t
  runStderrLoggingT $ P.withSqlitePool sqliteDB 10 $ \pool -> liftIO $ do
  warp port $ Browser pool
    where
      toint s = read s :: Int
