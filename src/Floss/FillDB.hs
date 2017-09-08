{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

module Floss.FillDB where

import Data.Text
import Database.Persist
import Database.Persist.Sqlite
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource.Internal

import Floss.DB
import Floss.Parser
import Floss.Query

import Network.HTTP.Client
import Network.HTTP.Client.TLS

insertsoftware :: Software -> ReaderT SqlBackend
                       (Control.Monad.Logger.NoLoggingT
                          (Control.Monad.Trans.Resource.Internal.ResourceT IO))
                       ()
insertsoftware = ap (repsert . qidtokey . qid) project

insertsoftware'
  :: (PersistEntityBackend record ~ BaseBackend backend,
      PersistStoreWrite backend, MonadIO m,
      ToBackendKey SqlBackend record2, ToBackendKey SqlBackend record1,
      Integral a, Integral b, PersistEntity record) =>
          (Key record2 -> Key record1 -> record) -> a -> Maybe b -> ReaderT backend m ()
insertsoftware' _     _   Nothing    = return ()
insertsoftware' con qId (Just oId) = insert_ $ con (qidtokey qId) (qidtokey oId)

insertall :: ItemList
             -> ReaderT SqlBackend (NoLoggingT (ResourceT IO)) ()
insertall (Collection l) = do
    mapM_ insertsoftware l
    zipWithM_ (insertsoftware' ProjectCoding)  (qid <$> l) (coding  <$> l)
    zipWithM_ (insertsoftware' ProjectLicense) (qid <$> l) (license <$> l)
    zipWithM_ (insertsoftware' ProjectOs)      (qid <$> l) (os      <$> l)
insertall _  = return ()

insertItemLabelList
  :: (PersistEntityBackend record ~ BaseBackend backend,
      PersistStoreWrite backend, MonadIO m, ToBackendKey SqlBackend record) =>
     (Maybe Text -> record) -> ItemList -> ReaderT backend m ()
insertItemLabelList con (ItemList l) = mapM_ insertitemlabel l
    where insertitemlabel i = repsert (qidtokey $ iqid i) (con $ iname i)
insertItemLabelList _     _ = return ()


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
