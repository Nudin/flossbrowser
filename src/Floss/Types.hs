{-# LANGUAGE OverloadedStrings #-}

module Floss.Types where

import Data.Text

type URL = Text

type WikidataItemID = Int

-- Convert String to WikidataItemID
-- fallback -1
strtoid :: String -> WikidataItemID
strtoid = strtoid_ where
  strtoid_ :: String -> Int
  strtoid_ "" = -1
  strtoid_ s = read s

-- Convert IRI of Wikidata-Item to an Int-ID
urltoid :: String -> WikidataItemID
urltoid = strtoid . Prelude.drop 32

