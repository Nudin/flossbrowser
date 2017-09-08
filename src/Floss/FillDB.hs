{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

module Floss.FillDB where

import Data.Text
import Database.Persist
import Database.Persist.Sqlite
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader

import Floss.DB
import Floss.Parser
import Floss.Query

import Network.HTTP.Client
import Network.HTTP.Client.TLS

insertsoftware = ap (repsert . qidtokey . qid) project

insertsoftware'
  :: (PersistEntityBackend record ~ BaseBackend backend,
      PersistStoreWrite backend, MonadIO m,
      ToBackendKey SqlBackend record2, ToBackendKey SqlBackend record1,
      Integral a, Integral b, PersistEntity record) =>
          (Key record2 -> Key record1 -> record) -> a -> Maybe b -> ReaderT backend m ()
insertsoftware' const qid (Just oid) = insert_ $ const (qidtokey qid) (qidtokey oid)
insertsoftware' _ _ Nothing = return ()

insertall (Collection l) = do
    mapM_ insertsoftware l
    zipWithM_ (insertsoftware' ProjectCoding)  (qid <$> l) (coding  <$> l)
    zipWithM_ (insertsoftware' ProjectLicense) (qid <$> l) (license <$> l)
    zipWithM_ (insertsoftware' ProjectOs)      (qid <$> l) (os      <$> l)
insertall Empty = return ()

insertItemLabelList
  :: (PersistEntityBackend record ~ BaseBackend backend,
      PersistStoreWrite backend, MonadIO m, ToBackendKey SqlBackend record) =>
     (Maybe Text -> record) -> ItemList -> ReaderT backend m ()
insertItemLabelList _ Empty = return ()
insertItemLabelList const (ItemList l) = mapM_ insertitemlabel l
    where insertitemlabel i = repsert (qidtokey $ iqid i) (const $ iname i)


initDB :: IO ()
initDB = runSqlite sqliteDB $ do
    runMigration migrateAll
    manager <- liftIO $ newManager tlsManagerSettings
    l   <- liftIO  $ getResource queryLicense manager
    insertItemLabelList License l
    c   <- liftIO  $ getResource queryCodings manager
    insertItemLabelList Coding c
    o   <- liftIO  $ getResource queryOs manager
    insertItemLabelList Os o

    col <- liftIO  $ getResource query manager
    insertall col
    return ()
