module Floss.Types where

import Data.Text
import Data.ByteString

type URL = Text

type WikidataItemID = Int

data BackendType = Sqlite | MySQL


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
