-- URLtoID.hs
module URLtoID(urltoid, WikidataItemID) where

-- TODO: Decide newtype or data or no new type at all?
--data WikidataItemID = WikidataItemID Int deriving (Show, Eq)
newtype WikidataItemID = WikidataItemID Int deriving (Show, Eq)

-- Convert String to Int
-- fallback -1 ← should we panic instead?
strtoid :: String -> WikidataItemID
strtoid = WikidataItemID . strtoid_ where
  strtoid_ :: String -> Int
  strtoid_ "" = -1
  strtoid_ s = read s

-- Convert IRI of Wikidata-Item to an Int-ID
urltoid :: String -> WikidataItemID
urltoid = strtoid . drop 32
