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
import Data.Text.Encoding
import Data.Time
import Database.Persist
import Database.Persist.Sql
import qualified Database.Persist.Sqlite as Sqlite hiding (toSqlKey)
import qualified Database.Persist.MySQL  as MySQL hiding (toSqlKey)
import Database.Persist.TH
import GHC.Int

import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Control
import Data.Pool

import Floss.Types

withDBPool :: (MonadBaseControl IO m, MonadIO m, MonadLogger m,
              BaseBackend backend ~ SqlBackend, IsSqlBackend backend)
              => FlossEnv -> (Data.Pool.Pool backend -> m a) -> m a
withDBPool env f =
    case backend env of
      Sqlite -> Sqlite.withSqlitePool (sqlFile env) 100 f
      MySQL  -> MySQL.withMySQLPool (MySQL.mkMySQLConnectInfo (unpack $ sqlHost env)
        (encodeUtf16BE $ sqlUser env) (encodeUtf16BE $ sqlPassword env) (encodeUtf16BE $ sqlDBName env)) 100 f

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
    repo URL Maybe
    fsd Text Maybe
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
Dev
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
ProjectDev
    pId ProjectId
    xId DevId
    deriving Show
|]


-- Deduce a db key from a WikiData Qxxxxx identifier
qidtokey :: (ToBackendKey SqlBackend record, Integral a) => a -> Key record
qidtokey qid = toSqlKey (fromIntegral qid :: Int64)
