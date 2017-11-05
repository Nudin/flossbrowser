{-# LANGUAGE   FlexibleContexts
             , GADTs
             , MultiParamTypeClasses
             , TypeFamilies #-}

module Floss.FillDB(initDB) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource.Internal
import Data.Text
import Database.Persist
import Database.Persist.Sql
import Network.HTTP.Client
import Network.HTTP.Client.TLS

import Floss.DB
import Floss.Parser
import Floss.Query

-- Insert the Software in the Projects-Table
insertsoftware :: Software -> ReaderT SqlBackend
                       (Control.Monad.Logger.NoLoggingT
                          (Control.Monad.Trans.Resource.Internal.ResourceT IO))
                       ()
insertsoftware = ap (repsert . qidtokey . qid) project

-- Inserts the additional info to the software in the given table
insertsoftware'
  :: (PersistEntityBackend record ~ BaseBackend backend,
      PersistStoreWrite backend, MonadIO m,
      ToBackendKey SqlBackend record2, ToBackendKey SqlBackend record1,
      Integral a, Integral b, PersistEntity record) =>
     (Key record2 -> Key record1 -> record) -> a -> Maybe b -> ReaderT backend m ()
insertsoftware' con qId (Just oId) = insert_ $ con (qidtokey qId) (qidtokey oId)
insertsoftware' _     _   Nothing  = return ()

-- Insert all records into the database
insertall :: ItemList
             -> ReaderT SqlBackend (NoLoggingT (ResourceT IO)) ()
insertall (Collection l) = do
    mapM_ insertsoftware l
    zipWithM_ (insertsoftware' ProjectCoding)  (qid <$> l) (coding  <$> l)
    zipWithM_ (insertsoftware' ProjectLicense) (qid <$> l) (license <$> l)
    zipWithM_ (insertsoftware' ProjectOs)      (qid <$> l) (os      <$> l)
    zipWithM_ (insertsoftware' ProjectGui)     (qid <$> l) (gui     <$> l)
    zipWithM_ (insertsoftware' ProjectCat)     (qid <$> l) (cat     <$> l)
    zipWithM_ (insertsoftware' ProjectDev)     (qid <$> l) (dev     <$> l)
insertall _  = return ()

insertItemLabelList
  :: (PersistEntityBackend record ~ BaseBackend backend,
      PersistStoreWrite backend, MonadIO m, ToBackendKey SqlBackend record) =>
     (Maybe Text -> record) -> ItemList -> ReaderT backend m ()
insertItemLabelList con (ItemList l) = mapM_ insertitemlabel l
    where insertitemlabel i = repsert (qidtokey $ iqid i) (con $ iname i)
insertItemLabelList _     _ = return ()


-- Receive, parse and store the data from WikiData
initDB :: IO ()
initDB = runStderrLoggingT $ filterLogger (\_ lvl -> lvl /= LevelDebug ) $
         withDBPool $ \pool -> liftIO $ flip runSqlPersistMPool pool $ do
    runMigration migrateAll
    manager <- liftIO $ newManager tlsManagerSettings
    insertLabels manager queryLicense License
    insertLabels manager queryCodings Coding
    insertLabels manager queryOs      Os
    insertLabels manager queryGui     Gui
    insertLabels manager queryCat     Cat
    insertLabels manager queryDev     Dev

    col <- liftIO  $ getResource query manager
    insertall col
    return ()
      where
        insertLabels manager q con = do
          l <- liftIO $ getResource q manager
          insertItemLabelList con l

