{-# LANGUAGE   OverloadedStrings #-}

module Floss.Types where

import Data.Text
import Data.ByteString
import Data.Configurator.Types as Conf

type URL = Text

type WikidataItemID = Int

data BackendType = Sqlite | MySQL

-- Make BackendType parsable by Configurator
instance Configured BackendType where
    convert (String a) =
        case toLower a of
            "sqlite" -> Just Sqlite
            "mysql"  -> Just MySQL
            _        -> Nothing
    convert _ = Nothing


data FlossEnv = FlossEnv {
  port        :: Int,
  root        :: Text,
  backend     :: BackendType,
  sqlUser     :: ByteString,
  sqlHost     :: String,
  sqlFile     :: Text,
  sqlPassword :: ByteString,
  sqlDBName   :: ByteString
}
