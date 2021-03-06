{-# LANGUAGE   DeriveGeneric
             , OverloadedStrings #-}

module Floss.Parser where

import Data.Aeson
import Data.Aeson.Types

import Data.Text
import Data.Time
import GHC.Generics
import Control.Monad
import Control.Applicative

import Floss.Types
import Floss.Str
import Floss.DB

-- Datatype for a Software item
data Software = Software {
  qid         :: !WikidataItemID,
  project     :: Project,
  os          :: Maybe WikidataItemID,
  coding      :: Maybe WikidataItemID,
  license     :: Maybe WikidataItemID,
  gui         :: Maybe WikidataItemID,
  cat         :: Maybe WikidataItemID,
  dev         :: Maybe WikidataItemID
  } deriving (Show, Generic)

data ItemLabel = ItemLabel {
  iqid  :: !WikidataItemID,
  iname :: Maybe Text
} deriving (Show, Generic)

-- Container types
data ItemList = ItemList [ItemLabel]
                | Collection [Software]
                | Empty
                deriving (Show, Generic)

newtype SPARQLResponse = SPARQLResponse ItemList
                    deriving (Show, Generic)

parseId :: Text -> Object -> Parser WikidataItemID
parseId field o = do
  result <- o .: field
  val <- result .: "value"
  return $ urltoid val

parseMaybeText :: Text -> Object -> Parser (Maybe Text)
parseMaybeText field o = do
  result <- o .:? field
  mapM (.: "value") result

parseMaybeId :: Text -> Object -> Parser (Maybe WikidataItemID)
parseMaybeId field o = do
  result <- parseMaybeText field o
  return $ fmap (urltoid . unpack) result

parseMaybeDay :: Text -> Object -> Parser (Maybe Day)
parseMaybeDay field o = do
  result <- parseMaybeText field o
  return $ fmap unpack result >>= parseDay
    where
      parseDay :: String -> Maybe Day
      parseDay = parseTimeM True defaultTimeLocale "%FT%TZ"


-- Parsers for JSON objects
instance FromJSON Software where
    parseJSON (Object o) =
        Software <$> parseId "floss" o
                 <*> ( return Project
                    <*> parseMaybeText "name" o
                    <*> parseMaybeText "description" o
                    <*> parseMaybeText "website" o
                    <*> parseMaybeText "logo" o
                    <*> parseMaybeText "img" o
                    <*> parseMaybeText "version" o
                    <*> parseMaybeDay  "start" o
                    <*> parseMaybeText "repo" o
                    <*> parseMaybeText "fsd" o
                     )
                 <*> parseMaybeId "os" o
                 <*> parseMaybeId "language" o
                 <*> parseMaybeId "license" o
                 <*> parseMaybeId "gui" o
                 <*> parseMaybeId "cat" o
                 <*> parseMaybeId "dev" o
    parseJSON _ = mzero

instance FromJSON ItemLabel where
    parseJSON (Object o) =
        parseIDLabel ItemLabel "license"  <|>
        parseIDLabel ItemLabel "language" <|>
        parseIDLabel ItemLabel "os" <|>
        parseIDLabel ItemLabel "gui" <|> 
        parseIDLabel ItemLabel "dev" <|> 
        parseIDLabel ItemLabel "category"
      where
        parseIDLabel c s =
            c <$> parseId s o
              <*> parseMaybeText (s `append` "Label") o
    parseJSON _ = mzero

instance FromJSON ItemList where
  parseJSON (Object o) = 
    ItemList <$> o .: "bindings" <|>
      Collection <$> o .: "bindings"
  parseJSON _ = mzero

instance FromJSON SPARQLResponse where
  parseJSON (Object o) = SPARQLResponse <$> o .: "results"
  parseJSON _ = mzero

