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

-- DB Schema
share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Project
    name Text Maybe
--    description Text
    link String Maybe
--    logo URL
--    img  URL
    deriving Show
License
    name Text
--    text Text
    deriving Show
Coding
    name Text
    deriving Show
ProjectCoding
    fkProjectId Int
    fkCodingId Int
    --UniqueMatch fkProjectId fkCodingId
    deriving Show
ProjectLicense
    fkProjectId Int
    fkLicenseId Int
    deriving Show
|]

sqliteDB = "../test.sql"


mkYesod
  "MyApp"
  [parseRoutes|
  /                 HomeR     GET
  /software/#String SoftwareR GET
  /foo              FooR      GET
|]

getHomeR :: Handler Html
getHomeR =
  defaultLayout $ do
    setTitle "Floss-Browser"
    toWidget
      [whamlet|
      <h1>Hello Yesod!1!
      Some text that 
      is <i>displayed</i> here.
      
      <p> This is a link to 
        <a href=@{FooR}>foo 
  |]

getSoftwareR :: String -> Handler Html
getSoftwareR software =
  defaultLayout $ do
    setTitle $ toHtml software
    toWidget $(luciusFile "./foo.lucius")
    toWidget $(hamletFile "./foo.hamlet")
    runSqlite sqliteDB $ do 
      runMigration migrateAll
      test <- selectList [ ProjectName ==. (Just $ pack software) ]  [LimitTo 1]
      return ()

getFooR :: Handler Html
getFooR =
  defaultLayout $ do
    setTitle "Foo"
    toWidget $(luciusFile "./foo.lucius")
    toWidget $(hamletFile "./foo.hamlet")

main :: IO ()
main = warp 3000 MyApp
