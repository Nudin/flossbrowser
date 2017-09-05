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

insertsoftware = ap (repsert . qidtokey . qid) (liftM3 Project name description website)

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
    zipWithM_ insertsoftwarecoding (qid <$> l) (coding <$> l)
    zipWithM_ insertsoftwarelicense (qid <$> l) (license <$> l)
    zipWithM_ insertsoftwareos (qid <$> l) (os <$> l)

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


insertsystem :: MonadIO m =>
    Os' -> ReaderT SqlBackend m ()
insertsystem c = repsert (qidtokey $ oqid c) (Os $ oname c)

insertsystems :: MonadIO m =>
    OsList -> ReaderT SqlBackend m ()
insertsystems (OsList c) = mapM_ insertsystem c


initDB :: IO ()
initDB = runSqlite sqliteDB $ do
    runMigration migrateAll
    manager <- liftIO $ newManager tlsManagerSettings
    l   <- liftIO $ getResource queryLicense manager
    insertlicenses $ fromResource l
    c   <- liftIO $ getResource queryCodings manager
    insertcodings $ fromResource c
    o   <- liftIO $ getResource queryOs manager
    insertsystems $ fromResource o
    col <- liftIO $ getResource query manager
    insertall $ fromResource col
    return ()
