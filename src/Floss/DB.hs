{-# LANGUAGE   EmptyDataDecls
             , FlexibleContexts
             , GADTs
             , GeneralizedNewtypeDeriving
             , MultiParamTypeClasses
             , OverloadedStrings
             , QuasiQuotes
             , TemplateHaskell
             , TypeFamilies #-}

module Floss.DB where

import Data.Text
import Data.Time
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import GHC.Int

import Floss.Types


sqliteDB :: Text
sqliteDB = "file:flossbrowser.sqlite"

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
Gui
    name Text Maybe
    deriving Show
Cat
    name Text Maybe
    deriving Show
ProjectOs
    pId ProjectId
    xId OsId
    deriving Show
ProjectCoding
    pId ProjectId
    xId CodingId
    deriving Show
ProjectLicense
    pId ProjectId
    xId LicenseId
    deriving Show
ProjectGui
    pId ProjectId
    xId GuiId
    deriving Show
ProjectCat
    pId ProjectId
    xId CatId
    deriving Show
|]

-- Deduce a db key from a WikiData Qxxxxx identifier
qidtokey :: (ToBackendKey SqlBackend record, Integral a) => a -> Key record
qidtokey qid = toSqlKey (fromIntegral qid :: Int64)
