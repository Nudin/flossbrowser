{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

module Floss.FillDB where

import Database.Persist
import Database.Persist.Sqlite
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader

import Floss.DB
import Floss.Types
import Floss.Query

import Network.HTTP.Client
import Network.HTTP.Client.TLS

insertsoftware = ap (repsert . qidtokey . qid) (liftM2 Project name website)

insertsoftwarecoding :: (MonadIO m, PersistStoreWrite backend,
                         BaseBackend backend ~ SqlBackend) =>
                         Int -> Maybe Int -> ReaderT backend m ()
insertsoftwarecoding qid (Just cid) = insert_ $ ProjectCoding (qidtokey qid) (qidtokey cid)
insertsoftwarecoding _ Nothing = return ()

insertsoftwarelicense qid (Just lid) = insert_ $ ProjectLicense (qidtokey qid) (qidtokey lid)
insertsoftwarelicense _ Nothing = return ()

insertall (Collection l) = do
    mapM_ insertsoftware l
    zipWithM_ insertsoftwarecoding (qid <$> l) (coding <$> l)
    zipWithM_ insertsoftwarelicense (qid <$> l) (license <$> l)

insertlicense :: MonadIO m =>
    License' -> ReaderT SqlBackend m ()
insertlicense l = repsert (qidtokey $ lqid l) (License $ lname l)

insertlicenses :: MonadIO m =>
    LicenseList -> ReaderT SqlBackend m ()
insertlicenses (LicenseList l) = mapM_ insertlicense l


insertcoding :: MonadIO m =>
    Coding' -> ReaderT SqlBackend m ()
insertcoding c = repsert (qidtokey $ cqid c) (Coding $ cname c)

insertcodings :: MonadIO m =>
    CodingList -> ReaderT SqlBackend m ()
insertcodings (CodingList c) = mapM_ insertcoding c


initDB :: IO ()
initDB = runSqlite sqliteDB $ do
    runMigration migrateAll
    manager <- liftIO $ newManager tlsManagerSettings
    col <- liftIO $ getCollection manager
    insertall col
    l   <- liftIO $ getLicenses manager
    insertlicenses l
    c   <- liftIO $ getCodings manager
    insertcodings c
    return ()
