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

-- TODO think about using Network.URL instead
type URL = Text

-- TODO: Decide type, newtype, data or no new type at all?
--data WikidataItemID = WikidataItemID Int deriving (Show, Eq)
type WikidataItemID = Int

-- Convert String to WikidataItemID
-- fallback -1 ← should we panic instead?
strtoid :: String -> WikidataItemID
strtoid = strtoid_ where
  strtoid_ :: String -> Int
  strtoid_ "" = -1
  strtoid_ s = read s

-- Convert IRI of Wikidata-Item to an Int-ID
urltoid :: String -> WikidataItemID
urltoid = strtoid . Prelude.drop 32

