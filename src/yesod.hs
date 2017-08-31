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

module Main where

import Floss.DB

import Text.Hamlet
import Text.Lucius
import Yesod

import Data.Text
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

data MyApp =
  MyApp

instance Yesod MyApp


mkYesod
  "MyApp"
  [parseRoutes|
    /                          HomeR        GET
    /software/#String          SoftwareR    GET
--    /softwarebyid/#ProjectId   SoftwareIdR  GET
|]

getHomeR :: Handler Html
getHomeR =
  defaultLayout $ do
    results <- runDB $ selectList []  [LimitTo 100]
    setTitle "Floss-Browser"
    toWidget $(hamletFile "./templates/softwarelist.hamlet")
      where -- TODO: deduplicate function-def
        runDB action = runSqlite (pack sqliteDB) $ (runMigration migrateAll >> action)

getSoftwareR :: String -> Handler Html
getSoftwareR software =
  defaultLayout $ do
    results <- runDB $ selectList [ ProjectName ==. (Just $ pack software) ]  [LimitTo 1]
    liftIO $ print $ results
    setTitle $ toHtml $ software ++ " "         -- TODO
    --toWidget $(luciusFile "./foo.lucius")     -- TODO
    toWidget $(hamletFile "./templates/software.hamlet")
      where
        runDB action = runSqlite (pack sqliteDB) $ (runMigration migrateAll >> action)

main :: IO ()
main = warp 3000 MyApp
