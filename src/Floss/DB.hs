
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
import Data.Time
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import GHC.Int

import Floss.Types


sqliteDB :: Text
sqliteDB = "file:test.sql"

sqliteDBro :: Text
sqliteDBro = append sqliteDB "?mode=ro"

-- DB Schema
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Project
    name Text Maybe
    description Text Maybe
    link URL Maybe
    logo URL Maybe
    img  URL Maybe
    version Text Maybe
    start Day Maybe
    deriving Show
Os
    name Text Maybe
    deriving Show
License
    name Text Maybe
    deriving Show
Coding
    name Text Maybe
    deriving Show
ProjectOs
    fkProjectId ProjectId
    fkOsId OsId
    deriving Show
ProjectCoding
    fkProjectId ProjectId
    fkCodingId CodingId
    --UniqueMatch fkProjectId fkCodingId
    deriving Show
ProjectLicense
    fkProjectId ProjectId
    fkLicenseId LicenseId
    deriving Show
|]

qidtokey :: (ToBackendKey SqlBackend record, Integral a) => a -> Key record
qidtokey qid = toSqlKey (fromIntegral qid :: Int64)
