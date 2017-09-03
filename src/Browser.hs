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
    /softwarebyid/#Int         SoftwareIdR   GET

    /bylicense/#String         ByLicenseR    GET
    /bycoding/#String          ByCodingR     GET

    !/#String                   Filter1R     GET
    !/#String/#String           Filter2R     GET
    !/#String/#String/#String   Filter3R     GET
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

inlineif t a b = if t then a else b

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
chooser :: String -> String -> Widget
chooser license coding = do
    ll <- handlerToWidget $ licenselist
    cl <- handlerToWidget $ codinglist
    toWidget
      [hamlet|
       <form action="#">
           <label> Lizenzen&#32
               <select name="license" id="licensechooser" onclick="filter()">
                   <option>-- all --
                   $forall Entity licenseid license' <- ll
                     $with wikidataid <- fromSqlKey licenseid
                       $maybe name <- (licenseName license')
                           <option :(license == (unpack name)):selected>
                                 #{name}
           <label> Programiersprachen&#32
               <select name="coding" id="codingchooser" onclick="filter()">
                   <option>-- all --
                   $forall Entity codingid coding' <- cl
                       $with wikidataid <- fromSqlKey codingid
                           $maybe name <- (codingName coding')
                             <option :(coding == (unpack name)):selected>
                                 #{name}
      |]
    toWidget 
        [julius|
          function filter() { 
            license = document.getElementById("licensechooser").value;
            coding = document.getElementById("codingchooser").value;
            newurl="/*/"
            if(license === "-- all --") {
               newurl += "*/"
            }
            else {
                  newurl += license + "/"
            }
            if(coding === "-- all --") {
               newurl += "*/"
            }
            else {
                  newurl += coding + "/"
            }
            console.log(newurl);
            window.location.href = newurl;
            }
        |]

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

---- Recourse handlers – mostly doing the same things -----
---- TODO: remove duplicate code                      -----

-- List all Software
getHomeR :: Handler Html
--getHomeR = getFilterN "*" "*" "*" -- elegant but slow version
getHomeR = do
    results <- runDB $ P.selectList []  [P.LimitTo 50]
    defaultLayout $ do
       setTitle "Floss-Browser"
       let coding = "*"
       let license = "*"
       toWidget $(whamletFile "./templates/softwarelist.hamlet")
       toWidget $(luciusFile "./templates/softwarelist.lucius")

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

getFilterN :: String -> String -> String -> Handler Html
getFilterN os license coding = do
    results <- runquery (check license) (check coding)
    defaultLayout $ do
      setTitle $ toHtml $ "Floss-Browser: Software licensed with license " ++ license
      toWidget $(whamletFile "./templates/softwarelist.hamlet")
      toWidget $(luciusFile "./templates/softwarelist.lucius")
    where
      check "*" = Nothing
      check s   = Just s

-- Get Software by OS
getFilter1R :: String -> Handler Html
getFilter1R os = getFilterN os "*" "*"

-- Get Software by OS & License
getFilter2R :: String -> String -> Handler Html
getFilter2R os license = getFilterN os license "*"

-- Get Software by OS, License & Coding
getFilter3R :: String -> String -> String -> Handler Html
getFilter3R = getFilterN

-- Get Software by License-Name
getByLicenseR :: String -> Handler Html
getByLicenseR license = getFilterN "*" license "*"

-- Get Software my Coding-Name
getByCodingR :: String -> Handler Html
getByCodingR coding = getFilterN "*" "*" coding

main :: IO ()
main = do
  t <- lookupEnv "PORT"
  let port = fromMaybe 3000 $ toint <$> t
  runStderrLoggingT $ P.withSqlitePool sqliteDB 10 $ \pool -> liftIO $ do
  warp port $ Browser pool
    where
      toint s = read s :: Int
