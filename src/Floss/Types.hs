{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Floss.Types where

import Data.Aeson
import Data.Aeson.Types

import Data.Text
import GHC.Generics
import Control.Monad
import Control.Applicative
import qualified Data.Generics as Gen

-- Datatype for a Software item
data Software = Software {
  qid         :: !WikidataItemID,
  name        :: Maybe Text,
  description :: Maybe Text,
  os          :: Maybe WikidataItemID,
  coding      :: Maybe WikidataItemID,
  license     :: Maybe WikidataItemID,
  website     :: Maybe Text,
  version     :: Maybe Text
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
maybeValue :: Text -> Object -> Parser (Maybe Text)
maybeValue field o = do
  result <- o .:? field
  mapM (.: "value") result

{- Mixed applicative and monad version -}
instance FromJSON Software where
    parseJSON (Object o) =
        Software <$> do project <- o .:  "floss"
                        projectiri <- project      .:  "value"
                        return $ urltoid projectiri
                 <*> maybeValue "name" o
                 <*> maybeValue "description" o
                 <*> do
                   x <- maybeValue "os" o
                   return $ fmap (urltoid . unpack) x
                 <*> do
                   x <- maybeValue "language" o
                   return $ fmap (urltoid . unpack) x
                 <*> do
                   x <- maybeValue "license" o
                   return $ fmap (urltoid . unpack) x
                 <*> maybeValue "website" o
                 <*> maybeValue "version" o
    parseJSON _ = mzero


--TODO rename
test c s o =
    c <$> do id <- o .: s
             iri <- id .: "value"
             return $ urltoid iri
      <*> maybeValue (s `Data.Text.append`  "Label") o
test _ _ _ = mzero

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


-- TODO think about using Network.URL instead
type URL = Text

-- TODO: Decide type, newtype, data or no new type at all?
--data WikidataItemID = WikidataItemID Int deriving (Show, Eq)
type WikidataItemID = Int

-- Convert String to Int
-- fallback -1 ← should we panic instead?
strtoid :: String -> WikidataItemID
strtoid = strtoid_ where
  strtoid_ :: String -> Int
  strtoid_ "" = -1
  strtoid_ s = read s

-- Convert IRI of Wikidata-Item to an Int-ID
urltoid :: String -> WikidataItemID
urltoid = strtoid . Prelude.drop 32

