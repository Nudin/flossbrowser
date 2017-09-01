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
insertsoftwarecoding qid (Just lid) = insert_ $ ProjectCoding (qidtokey qid) (qidtokey lid)
insertsoftwarecoding _ Nothing = return ()

insertall (Collection l) = do
    mapM_ insertsoftware l
    zipWithM_ insertsoftwarecoding (qid <$> l) (language <$> l)

insertlicense :: MonadIO m =>
    License' -> ReaderT SqlBackend m ()
insertlicense l = repsert (qidtokey $ lqid l) (License $ lname l)

insertlicenses :: MonadIO m =>
    LicenseList -> ReaderT SqlBackend m ()
insertlicenses (LicenseList l) = mapM_ insertlicense l


initDB :: IO ()
initDB = runSqlite sqliteDB $ do
    runMigration migrateAll
    col <- liftIO getCollection
    insertall col
    lic <- liftIO getLicenses
    insertlicenses lic
    return ()
