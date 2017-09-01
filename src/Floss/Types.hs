{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Floss.Types where

import Data.Aeson
import Data.Aeson.Types

import Data.Text
import GHC.Generics
import Control.Monad
import qualified Data.Generics as Gen

-- Datatype for a Software item
data Software = Software {
  qid :: !WikidataItemID,
  name :: Maybe Text,
  language :: Maybe WikidataItemID,
  website :: Maybe Text,
  version :: Maybe Text
  } deriving (Show, Generic, Gen.Typeable, Gen.Data)

{- Inspired by:
 - https://stackoverflow.com/questions/22807619/systematically-applying-a-function-to-all-fields-of-a-haskell-record
 -}
merge :: Software -> Software -> Software
merge a b = let b'  = Gen.gzipWithT mergeText a b
                b'' = Gen.gzipWithT mergeText a b'
            in Gen.gzipWithT mergeText a b''

mergeText :: (Gen.Data a, Gen.Data b) => a -> b -> b
mergeText = Gen.mkQ id (\a -> Gen.mkT (\b -> append <$> a <*> b :: Maybe Text))



data Collection = Collection [Software] deriving (Show, Generic)
data SPARQLResponse = SPARQLResponse Collection deriving (Show, Generic)



{- Mixed applicative and monad version -}
instance FromJSON Software where
    parseJSON (Object o) =
        Software <$> do project <- o .:  "floss"
                        projectiri <- project      .:  "value"
                        return $ urltoid projectiri
                 <*> do name    <- o .:? "name" :: (Parser (Maybe Object))
                        case name of
                             Nothing  -> return Nothing
                             (Just x) -> x .: "value"
                 <*> do lang    <- o .:? "language" :: (Parser (Maybe Object))
                        case lang of
                             Nothing  -> return Nothing
                             (Just x) -> do
                                foo <- x .: "value"
                                return $ Just $ urltoid foo
                 <*> do web    <- o .:? "website" :: (Parser (Maybe Object))
                        case web of
                             Nothing  -> return Nothing
                             (Just x) -> x .: "value"
                 <*> do version    <- o .:? "version" :: (Parser (Maybe Object))
                        case version of

                             Nothing  -> return Nothing
                             (Just x) -> x .: "value"
    parseJSON _ = mzero





instance FromJSON Collection where
  parseJSON (Object o) =
    Collection <$> o .: "bindings"
  parseJSON _ = mzero

instance FromJSON SPARQLResponse where
  parseJSON (Object o) =
    SPARQLResponse <$> o .: "results"
  parseJSON _ = mzero

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
