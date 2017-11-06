{-# LANGUAGE ScopedTypeVariables #-}

import Floss.Types
import Floss.FillDB
import Floss.Config (readConfig)
import Control.Exception
import Control.Monad.Reader
import Network.HTTP.Client


type DBCreatorT m = ReaderT FlossEnv m
type DBCreatorIO  = DBCreatorT IO


main :: IO ()
main = do
    config <- readConfig
    runReaderT dbCreation config 

dbCreation :: DBCreatorIO ()
dbCreation = do
    env <- ask
    liftIO $ initDB env `catch` (\(e :: HttpException) -> handleHTTPEx e)


handleHTTPEx :: HttpException -> IO ()
handleHTTPEx _ = print "A network error occurred. Wikidata is probably busy, try again."
