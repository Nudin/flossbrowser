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
import Yesod

import Data.Text
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))


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

licenselist = runDB 
           $ E.select $ E.distinct
           $ E.from $ \(pl `E.InnerJoin` l) -> do
                E.on $ l ^. LicenseId E.==. pl ^. ProjectLicenseFkLicenseId
                E.limit 50
                E.orderBy [ E.asc (l ^. LicenseName) ]
                return l

getHomeR :: Handler Html
getHomeR = do
    results <- runDB $ selectList []  [LimitTo 50]
    ll <- licenselist
    defaultLayout $ do
       setTitle "Floss-Browser"
       softwarelist results ll

getSoftwareR :: String -> Handler Html
getSoftwareR software = do
    results <- runDB $ selectList [ ProjectName ==. (Just $ pack software) ]  [LimitTo 1]
    liftIO $ print $ results
    defaultLayout $ do
      setTitle $ toHtml $ "Flossbrowser: " ++ software
      toWidget $(whamletFile "./templates/software.hamlet")
      --toWidget $(luciusFile "./templates/software.lucius")

getByLicenseIdR :: Int -> Handler Html
getByLicenseIdR license = do
    ll <- licenselist
    results <- runDB
           $ E.select $ E.distinct
           $ E.from $ \(p `E.InnerJoin` pl) -> do
                E.on $ p ^. ProjectId E.==. pl ^. ProjectLicenseFkProjectId
                E.where_ ( pl ^. ProjectLicenseFkLicenseId E.==. E.val (qidtokey license) )
                E.limit 50
                return p
    defaultLayout $ do
       setTitle "Floss-Browser"
       softwarelist results ll

getByLicenseR :: String -> Handler Html
getByLicenseR license = do
    ll <- licenselist
    results <- runDB
           $ E.select $ E.distinct
           $ E.from $ \(p `E.InnerJoin` pl `E.InnerJoin` l) -> do
                E.on $ p ^. ProjectId E.==. pl ^. ProjectLicenseFkProjectId
                E.on $ l ^. LicenseId E.==. pl ^. ProjectLicenseFkLicenseId
                E.where_ ( l ^. LicenseName E.==. E.val (Just (pack license)) )
                E.limit 50
                return p
    defaultLayout $ do
       setTitle "Floss-Browser"
       softwarelist results ll

getByCodingIdR :: Int -> Handler Html
getByCodingIdR coding = do
    ll <- licenselist
    results <- runDB
           $ E.select $ E.distinct
           $ E.from $ \(p `E.InnerJoin` pc) -> do
                E.on $ p ^. ProjectId E.==. pc ^. ProjectCodingFkProjectId
                E.where_ ( pc ^. ProjectCodingFkCodingId E.==. E.val (qidtokey coding) )
                E.limit 50
                return p
    defaultLayout $ do
       setTitle "Floss-Browser"
       softwarelist results ll

getByCodingR :: String -> Handler Html
getByCodingR coding = do
    ll <- licenselist
    results <- runDB
           $ E.select $ E.distinct
           $ E.from $ \(p `E.InnerJoin` pc `E.InnerJoin` c) -> do
                E.on $ p ^. ProjectId E.==. pc ^. ProjectCodingFkProjectId
                E.on $ c ^. CodingId E.==. pc ^. ProjectCodingFkCodingId
                E.where_ ( c ^. CodingName E.==. E.val (Just (pack coding)) )
                E.limit 50
                return p
    defaultLayout $ do
       setTitle "Floss-Browser"
       softwarelist results ll


main :: IO ()
main = runStderrLoggingT $ withSqlitePool sqliteDB 10 $ \pool -> liftIO $ do
  warp 3000 $ Browser pool
