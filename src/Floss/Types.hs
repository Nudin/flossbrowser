module Floss.Types where

import Data.Text

type URL = Text

type WikidataItemID = Int

data BackendType = Sqlite | MySQL


data FlossEnv = FlossEnv {
  port        :: Int,
  root        :: Text,
  backend     :: BackendType,
  sqlUser     :: Text,
  sqlHost     :: Text,
  sqlFile     :: Text,
  sqlPassword :: Text,
  sqlDBName   :: Text
}
