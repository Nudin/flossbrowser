{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}


module Floss.Types where

import Data.Aeson
import Data.Aeson.Types

import Data.Text
import GHC.Generics
import Control.Monad

import URLtoID

-- Datatype for a Software item
data Software = Software {
  qid :: !WikidataItemID,
  name :: Maybe Text,
  language :: Maybe WikidataItemID,
  website :: Maybe Text,
  version :: Maybe Text
  } deriving (Show, Generic)

-- Datatype for a Software item with arrays
data ComplexSoftware = ComplexSoftware {
  cid :: !WikidataItemID,
  clanguage :: Maybe [Text]
  } deriving (Show, Generic)

data Collection = Collection [Software] deriving (Show, Generic)
data SPARQLResponse = SPARQLResponse Collection deriving (Show, Generic)

{- Monad version -}
{-
 -instance FromJSON Software where
 -    parseJSON (Object o) = do
 -        projectO <- o        .:  "floss"
 -        project  <- projectO .:  "value"
 -        langO  <- o .:? "language" :: (Parser (Maybe Object))
 -        lang   <- case langO of
 -                      Nothing  -> return Nothing
 -                      (Just x) -> x .: "value"
 -        return $ Software project lang
 -    parseJSON _ = mzero
 -}

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

{- Mixed applicative and monad version, with array -}
instance FromJSON ComplexSoftware where
    parseJSON (Object o) =
        ComplexSoftware <$> do project <- o .:  "floss"
                               projectiri <- project      .:  "value"
                               return $ urltoid projectiri
                 <*> do lang    <- o .:? "language" :: (Parser (Maybe Object))
                        case lang of
                             Nothing  -> return Nothing
                             (Just x) -> do 
                                foo <- x .: "value" :: (Parser (Maybe Text))
                                return $ sequence $ foo : []
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
