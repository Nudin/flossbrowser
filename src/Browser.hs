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
--    /softwarebyid/#ProjectId   SoftwareIdR  GET
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

softwarelist results ll = do
       toWidget $(whamletFile "./templates/softwarelist.hamlet")
       toWidget $(luciusFile "./templates/softwarelist.lucius")
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

licenselist = runDB 
           $ select $ distinct
           $ from $ \(pl `InnerJoin` l) -> do
                on $ l ^. LicenseId ==. pl ^. ProjectLicenseFkLicenseId
                limit 50
                orderBy [ asc (l ^. LicenseName) ]
                return l

getHomeR :: Handler Html
getHomeR = do
    results <- runDB $ P.selectList []  [P.LimitTo 50]
    ll <- licenselist
    defaultLayout $ do
       setTitle "Floss-Browser"
       softwarelist results ll

getSoftwareR :: String -> Handler Html
getSoftwareR software = do
    results <- runDB $ P.selectList [ ProjectName P.==. (Just $ pack software) ]  [P.LimitTo 1]
    liftIO $ print $ results
    defaultLayout $ do
      setTitle $ toHtml $ "Flossbrowser: " ++ software
      toWidget $(whamletFile "./templates/software.hamlet")
      --toWidget $(luciusFile "./templates/software.lucius")

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
