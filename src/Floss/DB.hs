{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Floss.DB where

import Data.Text
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

import Floss.Types

-- DB Schema
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Project
    name Text
    description Text
    link URL
    logo URL
    img  URL
    fkLang Int
    fkLicense Int
    deriving Show
License
    name Text
    text Text
    deriving Show
Coding
    name Text
    deriving Show
ProjectCoding
    fkProjectId Int
    fkCodingId Int
    deriving Show
ProjectLicense
    fkProjectId Int
    fkLicenseId Int
    deriving Show
|]

initDB :: IO ()
initDB = runSqlite ":memory:" $ runMigration migrateAll
