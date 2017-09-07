{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Floss.Parser where

import Data.Aeson
import Data.Aeson.Types

import Data.Text
import Data.Time
import GHC.Generics
import Control.Monad
import Control.Applicative
import qualified Data.Generics as Gen

import Floss.Types
import Floss.DB

-- Datatype for a Software item
data Software = Software {
  qid         :: !WikidataItemID,
  project     :: Project,
  os          :: Maybe WikidataItemID,
  coding      :: Maybe WikidataItemID,
  license     :: Maybe WikidataItemID
  } deriving (Show, Generic)

data License' = License' {
  lqid  :: !WikidataItemID,
  lname :: Maybe Text
} deriving (Show, Generic)

data Coding' = Coding' {
  cqid  :: !WikidataItemID,
  cname :: Maybe Text
} deriving (Show, Generic)

data Os' = Os' {
  oqid  :: !WikidataItemID,
  oname :: Maybe Text
} deriving (Show, Generic)

newtype Collection = Collection [Software] deriving (Show, Generic)
newtype LicenseList = LicenseList [License'] deriving (Show, Generic) 
newtype CodingList = CodingList [Coding'] deriving (Show, Generic) 
newtype OsList = OsList [Os'] deriving (Show, Generic) 

data SPARQLResponse = SPARQLResponse Collection
                    | SPARQLResponseLicenses LicenseList
                    | SPARQLResponseCodings CodingList
                    | SPARQLResponseOs OsList
                    deriving (Show, Generic)

-- Can't we do this more idiomatic? Or at least prettier?
parseId :: Text -> Object -> Parser (WikidataItemID)
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
  return $ (fmap unpack result) >>= parseDay
    where
      parseDay :: String -> Maybe Day
      parseDay = parseTimeM True defaultTimeLocale "%FT%TZ"

--TODO rename
test c s o =
    c <$> parseId s o
      <*> parseMaybeText (s `Data.Text.append`  "Label") o
test _ _ _ = mzero


{- Mixed applicative and monad version -}
instance FromJSON Software where
    parseJSON (Object o) = do
        Software <$> parseId "floss" o
                 <*> ( return Project
                   <*> parseMaybeText "name" o
                   <*> parseMaybeText "description" o
                   <*> parseMaybeText "website" o
                   <*> parseMaybeText "logo" o
                   <*> parseMaybeText "img" o
                   <*> parseMaybeText "version" o
                   <*> parseMaybeDay "start" o
                     )
                 <*> parseMaybeId "os" o
                 <*> parseMaybeId "language" o
                 <*> parseMaybeId "license" o
    parseJSON _ = mzero

instance FromJSON License' where
    parseJSON (Object o) = test License' "license" o

instance FromJSON LicenseList where
  parseJSON (Object o) = LicenseList <$> o .: "bindings"
  parseJSON _ = mzero

instance FromJSON Os' where
    parseJSON (Object o) = test Os' "os" o

instance FromJSON Coding' where
    parseJSON (Object o) = test Coding' "language" o

instance FromJSON OsList where
  parseJSON (Object o) = OsList <$> o .: "bindings"
  parseJSON _ = mzero

instance FromJSON CodingList where
  parseJSON (Object o) = CodingList <$> o .: "bindings"
  parseJSON _ = mzero

instance FromJSON Collection where
  parseJSON (Object o) = Collection <$> o .: "bindings"
  parseJSON _ = mzero

instance FromJSON SPARQLResponse where
  parseJSON (Object o) =
    (SPARQLResponse <$> res o) <|>
      (SPARQLResponseOs <$> res o) <|>
      (SPARQLResponseLicenses <$> res o) <|>
      (SPARQLResponseCodings <$> res o)
      where
        res :: FromJSON a => Object -> Parser a
        res = flip (.:) "results"
  parseJSON _ = mzero


data Empty = Empty
newtype FlossResource a = FlossResource a

class FlossSource a where
    asResource :: a -> FlossResource a
    asResource = FlossResource
    fromResource :: FlossResource a -> a
    fromResource (FlossResource r) = r

instance FlossSource Collection
instance FlossSource LicenseList
instance FlossSource CodingList
instance FlossSource OsList
instance FlossSource Empty


class FlossSource a => FromSPARQL a where
    fromSPARQLResponse :: Maybe SPARQLResponse -> FlossResource a

instance FromSPARQL Collection where
    fromSPARQLResponse (Just (SPARQLResponse c@(Collection _))) = asResource c
instance FromSPARQL LicenseList where
    fromSPARQLResponse (Just (SPARQLResponseLicenses c@(LicenseList _))) = asResource c
instance FromSPARQL CodingList where
    fromSPARQLResponse (Just (SPARQLResponseCodings c@(CodingList _))) = asResource c
instance FromSPARQL OsList where
    fromSPARQLResponse (Just (SPARQLResponseOs c@(OsList _))) = asResource c
instance FromSPARQL Empty where
    fromSPARQLResponse Nothing = asResource Empty

