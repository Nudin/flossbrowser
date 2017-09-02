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


import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger (runStderrLoggingT)

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
licenselist = runDB 
           $ select $ distinct
           $ from $ \(pl `InnerJoin` l) -> do
                on $ l ^. LicenseId ==. pl ^. ProjectLicenseFkLicenseId
                limit 50
                orderBy [ asc (l ^. LicenseName) ]
                return l

-- Chooser, to allow filtering for License, etc.
-- For now it works via Page-Redirect and the Recource-Handler do the work
-- The list of what options are available is currently given as an argument
-- TODO: Is it possible to run the Database-Query in here?
-- TODO: Move to separate files
-- TODO: Add other filters
-- TODO: Preselect current value
chooser :: [Entity License] -> Widget
chooser ll = do
    toWidget
      [hamlet|
       <form action="#">
           <label>Gefundene Lizenzen
               <select name="license" id="licensechooser" onclick="chooselicense()">
                   <option>-- all --
                   $forall Entity licenseid license <- ll
                       <option>
                           $with wikidataid <- fromSqlKey licenseid
                               $maybe name <- (licenseName license)
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
        |]

-- Compose Hamlet- and Lucius-Template of the software list
softwarelist :: (HandlerSite m ~ Browser, MonadWidget m) =>
     [Entity Project] -> [Entity License] -> m ()
softwarelist results ll = do
       toWidget $(whamletFile "./templates/softwarelist.hamlet")
       toWidget $(luciusFile "./templates/softwarelist.lucius")


---- Recourse handlers – mostly doing the same things -----
---- TODO: remove duplicate code                      -----

-- List all Software
getHomeR :: Handler Html
getHomeR = do
    results <- runDB $ P.selectList []  [P.LimitTo 50]
    ll <- licenselist
    defaultLayout $ do
       setTitle "Floss-Browser"
       softwarelist results ll

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
    ll <- licenselist
    results <- runDB
           $ select $ distinct
           $ from $ \(p `InnerJoin` pl) -> do
                on $ p ^. ProjectId ==. pl ^. ProjectLicenseFkProjectId
                where_ ( pl ^. ProjectLicenseFkLicenseId ==. val (qidtokey license) )
                limit 50
                return p
    defaultLayout $ do
      setTitle $ toHtml $ "Floss-Browser: Software licensed with license Q" ++ (show license)
      softwarelist results ll

-- Get Software by License-Name
getByLicenseR :: String -> Handler Html
getByLicenseR license = do
    ll <- licenselist
    results <- runDB
           $ select $ distinct
           $ from $ \(p `InnerJoin` pl `InnerJoin` l) -> do
                on $ p ^. ProjectId ==. pl ^. ProjectLicenseFkProjectId
                on $ l ^. LicenseId ==. pl ^. ProjectLicenseFkLicenseId
                where_ ( l ^. LicenseName ==. val (Just (pack license)) )
                limit 50
                return p
    defaultLayout $ do
      setTitle $ toHtml $ "Floss-Browser: Software licensed with license " ++ license
      softwarelist results ll

-- Get Software my Coding-ID
getByCodingIdR :: Int -> Handler Html
getByCodingIdR coding = do
    ll <- licenselist
    results <- runDB
           $ select $ distinct
           $ from $ \(p `InnerJoin` pc) -> do
                on $ p ^. ProjectId ==. pc ^. ProjectCodingFkProjectId
                where_ ( pc ^. ProjectCodingFkCodingId ==. val (qidtokey coding) )
                limit 50
                return p
    defaultLayout $ do
      setTitle $ toHtml $ "Floss-Browser: Software written in Q" ++ (show coding)
      softwarelist results ll

-- Get Software my Coding-Name
getByCodingR :: String -> Handler Html
getByCodingR coding = do
    ll <- licenselist
    results <- runDB
           $ select $ distinct
           $ from $ \(p `InnerJoin` pc `InnerJoin` c) -> do
                on $ p ^. ProjectId ==. pc ^. ProjectCodingFkProjectId
                on $ c ^. CodingId ==. pc ^. ProjectCodingFkCodingId
                where_ ( c ^. CodingName ==. val (Just (pack coding)) )
                limit 50
                return p
    defaultLayout $ do
      setTitle $ toHtml $ "Floss-Browser: Software written in " ++ coding
      softwarelist results ll


main :: IO ()
main = runStderrLoggingT $ P.withSqlitePool sqliteDB 10 $ \pool -> liftIO $ do
  warp 3000 $ Browser pool
