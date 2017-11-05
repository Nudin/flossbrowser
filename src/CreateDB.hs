{-# LANGUAGE ScopedTypeVariables #-}

import Floss.Types
import Floss.FillDB
import Control.Exception
import Control.Monad.Reader

import Network.HTTP.Client

import Data.Configurator       as Conf
import Data.Configurator.Types as Conf

import Floss.Config (readConfig)

type DBCreatorT m = ReaderT FlossEnv m
type DBCreatorIO  = DBCreatorT IO


main :: IO ()
main = do
    config <- readConfig
    runDBCreatorT config dbCreation

runDBCreatorT :: Monad m => DBCreatorEnv -> DBCreatorT m a -> m a
runDBCreatorT = flip runReaderT

dbCreation :: DBCreatorIO ()
dbCreation = do
    env <- ask
    let dbType = backend env
    let user   = sqlUser env
    let host   = sqlHost env
    let file   = sqlFile env
    let pw     = sqlPassword env
    let db     = sqlDBName env
    liftIO $ initDB dbType `catch` (\(e :: HttpException) -> handleHTTPEx e)


handleHTTPEx :: HttpException -> IO ()
handleHTTPEx _ = print "A network error occurred. Wikidata is probably busy, try again."
