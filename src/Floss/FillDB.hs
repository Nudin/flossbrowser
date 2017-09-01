{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Floss.FillDB where

import Data.Text
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import GHC.Int
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader

import Floss.DB
import Floss.Types
import Floss.Query

--insertsoftware obj = repsert (qidtokey $ qid obj)  $ Project (name obj) (website obj)
-- ^original version, pointfree:
insertsoftware = ap (repsert . qidtokey . qid) (liftM2 Project name website)

--insertsoftwarecoding :: (MonadIO m, PersistStoreWrite backend,
--                         BaseBackend backend ~ SqlBackend) =>
--                         Int -> Maybe Int -> ReaderT backend m ()
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
    col <- liftIO getCollection
    insertall col
    l   <- liftIO getLicenses
    insertlicenses l
    c   <- liftIO getCodings
    insertcodings c
    return ()
