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
import qualified Database.Persist.Sqlite as Sqlite
import qualified Database.Persist.MySQL  as MySQL
import Database.Persist.TH
import GHC.Int

import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Control
import Data.Pool

import Floss.Types

sqliteDB :: Text
sqliteDB = "file:flossbrowser.sqlite"

sqliteDBro :: Text
sqliteDBro = append sqliteDB "?mode=ro"

connectionInfo :: MySQL.MySQLConnectInfo
connectionInfo = MySQL.mkMySQLConnectInfo "localhost" "username" "password" "flossbrowser"

{-withDBPool :: BackendType -> (MonadBaseControl IO m, MonadIO m, MonadLogger m,-}
                 {-BaseBackend backend ~ SqlBackend, IsSqlBackend backend)-}
             {-=> (Data.Pool.Pool backend -> m a) -> m a-}
withDBPool sqlt =
    case sqlt of
        Sqlite -> withSqlitePool sqliteDBro 100
        MySQL  -> MySQL.withMySQLPool connectionInfo 100

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

class SqlKey record where
    toSqlKey :: Integral a => a -> m record

instance SqlKey (Sqlite.ToBackendKey Sqlite.SqlBackend)
    where toSqlKey = Sqlite.toSqlKey

instance SqlKey (MySQL.ToBackendKey MySQL.SqlBackend)
    where toSqlKey = MySQL.toSqlKey

-- Deduce a db key from a WikiData Qxxxxx identifier
qidtokey :: (SqlKey record, Integral a) => a -> m record
qidtokey qid = toSqlKey (fromIntegral qid :: Int64)
