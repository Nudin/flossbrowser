{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}


module Floss.Types where

import Data.Aeson
import Data.Aeson.Types

import Data.Text
import GHC.Generics
import Control.Monad

-- Datatype for a Software item
data Software = Software {
  floss :: !Text,
  language :: Maybe Text
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
                        project      .:  "value"
                 <*> do lang    <- o .:? "language" :: (Parser (Maybe Object))
                        case lang of
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
