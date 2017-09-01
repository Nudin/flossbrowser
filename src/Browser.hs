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
    /bylicense/#Int          ByLicenseR  GET
    /bycoding/#Int          ByCodingR  GET
|]

instance YesodPersist Browser where
    type YesodPersistBackend Browser = SqlBackend
    runDB action = do
        Browser pool <- getYesod
        runSqlPool action pool

getHomeR :: Handler Html
getHomeR = do
    results <- runDB $ selectList []  [LimitTo 50]
    defaultLayout $ do
       setTitle "Floss-Browser"
       toWidget $(hamletFile "./templates/softwarelist.hamlet")
       toWidget $(luciusFile "./templates/softwarelist.lucius")     -- TODO

getSoftwareR :: String -> Handler Html
getSoftwareR software = do
    results <- runDB $ selectList [ ProjectName ==. (Just $ pack software) ]  [LimitTo 1]
    liftIO $ print $ results
    defaultLayout $ do
      setTitle $ toHtml $ software ++ " "         -- TODO
      --toWidget $(luciusFile "./foo.lucius")     -- TODO
      toWidget $(hamletFile "./templates/software.hamlet")
      toWidget $(luciusFile "./templates/softwarelist.lucius")     -- TODO

getByLicenseR :: Int -> Handler Html
getByLicenseR license = do
    results <- runDB
           $ E.select $ E.distinct
           $ E.from $ \(p `E.InnerJoin` pl) -> do
                E.on $ p ^. ProjectId E.==. pl ^. ProjectLicenseFkProjectId
                E.where_ ( pl ^. ProjectLicenseFkLicenseId E.==. E.val (qidtokey license) )
                E.limit 50
                return p
    defaultLayout $ do
       toWidget $(hamletFile "./templates/softwarelist.hamlet")
       toWidget $(luciusFile "./templates/softwarelist.lucius")     -- TODO
       setTitle "Floss-Browser"

getByCodingR :: Int -> Handler Html
getByCodingR coding = do
    results <- runDB
           $ E.select $ E.distinct
           $ E.from $ \(p `E.InnerJoin` pc) -> do
                E.on $ p ^. ProjectId E.==. pc ^. ProjectCodingFkProjectId
                E.where_ ( pc ^. ProjectCodingFkCodingId E.==. E.val (qidtokey coding) )
                E.limit 50
                return p
    defaultLayout $ do
       toWidget $(hamletFile "./templates/softwarelist.hamlet")
       toWidget $(luciusFile "./templates/softwarelist.lucius")     -- TODO
       setTitle "Floss-Browser"


main :: IO ()
main = runStderrLoggingT $ withSqlitePool sqliteDB 10 $ \pool -> liftIO $ do
  warp 3000 $ Browser pool
