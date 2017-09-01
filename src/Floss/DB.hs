
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
import GHC.Int
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader

import Floss.Types
import Floss.Query


sqliteDB :: Text
sqliteDB = "./test.sql"

-- DB Schema
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Project
    name Text Maybe
--    description Text
    link Text Maybe
--    logo URL
--    img  URL
    deriving Show
License
    name Text Maybe
    deriving Show
Coding
    name Text Maybe
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
