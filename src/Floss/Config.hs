{-# LANGUAGE   OverloadedStrings #-}

module Floss.Config where

import Data.Configurator       as Conf
import Data.Configurator.Types as Conf

import Floss.Types


confSettings :: AutoConfig
confSettings  = AutoConfig { interval = 10,
                             onError  = handleConfError }

handleConfError :: Show t => t -> IO ()
handleConfError e = print $ "Error reading config file: " ++ show e


readConfig :: IO FlossEnv
readConfig = do
    (conf, _) <- autoReload confSettings [Required "./flossrc"]
    p <- Conf.lookupDefault 3000 conf "port"
    r <- Conf.lookupDefault ""  conf "root"
    dbt  <- Conf.lookupDefault Sqlite conf "backend"
    user <- Conf.lookupDefault   ""   conf "user"
    host <- Conf.lookupDefault   ""   conf "host"
    file <- Conf.lookupDefault   ""   conf "dbfile"
    pw <- Conf.lookupDefault     ""   conf "password"
    dbname <- Conf.lookupDefault ""   conf "dbname"
    return FlossEnv { port = p,
                      root = r ,
                      backend = dbt,
                      sqlUser = user,
                      sqlHost = host,
                      sqlFile = file,
                      sqlPassword = pw,
                      sqlDBName = dbname }
