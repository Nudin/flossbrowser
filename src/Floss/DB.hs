{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Floss.DB where

import Data.Text
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import GHC.Int
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader

import Floss.Types
import Floss.Query

-- DB Schema
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Project
    name Text Maybe
--    description Text
    link URL Maybe
--    logo URL
--    img  URL
    deriving Show
License
    name Text
--    text Text
    deriving Show
Coding
    name Text
    deriving Show
ProjectCoding
    fkProjectId Int
    fkCodingId Int
    --UniqueMatch fkProjectId fkCodingId
    deriving Show
ProjectLicense
    fkProjectId Int
    fkLicenseId Int
    deriving Show
|]

qidtokey :: (ToBackendKey SqlBackend record, Integral a) => a -> Key record
qidtokey qid = toSqlKey (fromIntegral qid :: Int64)

--insertsoftware obj = repsert (qidtokey $ qid obj)  $ Project (name obj) (website obj)
-- ^original version, pointfree:
insertsoftware = ap (repsert . qidtokey . qid) (liftM2 Project name website)

insertsoftwarecoding :: (MonadIO m, PersistStoreWrite backend,
                         BaseBackend backend ~ SqlBackend) =>
                         Int -> Maybe Int -> ReaderT backend m ()
insertsoftwarecoding qid (Just lid) = insert_ $ ProjectCoding qid lid 
insertsoftwarecoding _ Nothing = return ()

insertall (Collection []) = return ()
insertall (Collection (x:xs)) = do
    insertsoftware x
    insertsoftwarecoding (qid x) (language x)
    insertall (Collection xs)

insertall' (Collection []) = return ()
insertall' (Collection l) = do
    mapM_ insertsoftware l
    mapM_ (uncurry insertsoftwarecoding) (Prelude.zip (qid <$> l) (language <$> l))

initDB :: IO ()
initDB = runSqlite "test.sql" $ do  -- replaced :memory: to test easier
    runMigration migrateAll
    col <- liftIO getCollection
    insertall col
    return ()
