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

insertsoftwarecoding :: (MonadIO m, PersistStoreWrite backend,
                         BaseBackend backend ~ SqlBackend) =>
                         Int -> Maybe Int -> ReaderT backend m ()
insertsoftwarecoding qid (Just cid) = insert_ $ ProjectCoding (qidtokey qid) (qidtokey cid)
insertsoftwarecoding _ Nothing = return ()

insertsoftwarelicense :: (MonadIO m, PersistStoreWrite backend,
                         BaseBackend backend ~ SqlBackend) =>
                         Int -> Maybe Int -> ReaderT backend m ()
insertsoftwarelicense qid (Just lid) = insert_ $ ProjectLicense (qidtokey qid) (qidtokey lid)
insertsoftwarelicense _ Nothing = return ()

insertsoftwareos :: (MonadIO m, PersistStoreWrite backend,
                         BaseBackend backend ~ SqlBackend) =>
                         Int -> Maybe Int -> ReaderT backend m ()
insertsoftwareos qid (Just oid) = insert_ $ ProjectOs (qidtokey qid) (qidtokey oid)
insertsoftwareos _ Nothing = return ()

insertall (Collection l) = do
    mapM_ insertsoftware l
    zipWithM_ insertsoftwarecoding  (qid <$> l) (coding  <$> l)
    zipWithM_ insertsoftwarelicense (qid <$> l) (license <$> l)
    zipWithM_ insertsoftwareos      (qid <$> l) (os      <$> l)
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
