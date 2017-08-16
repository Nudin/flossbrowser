{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Text
import Str(str)
import GHC.Generics
import Data.Aeson
import Data.Aeson.Types
import Control.Monad

import Network.URI(escapeURIString,isAllowedInURI)
import Network.HTTP.Conduit

url :: String
url = "https://query.wikidata.org/sparql?format=json&query="

query :: String
query = [str|SELECT DISTINCT ?floss ?language ?version ?website ?licence ?os WHERE {
  {
   ?floss p:P31/ps:P31/wdt:P279* wd:Q506883.
  } Union {
   ?floss p:P31/ps:P31/wdt:P279* wd:Q341.
  } Union {
   ?floss p:P31/ps:P31/wdt:P279* wd:Q1130645.
  } Union {
   ?floss p:P31/ps:P31/wdt:P279* wd:Q19652.
   ?floss p:P31/ps:P31/wdt:P279* wd:Q7397.
  } Union {
    ?floss p:P31/ps:P31/wdt:P279* wd:Q7397.
    ?floss wdt:P275 ?licens.
    ?licens p:P31/ps:P31/(wdt:P31|wdt:P279)* ?kind.
    VALUES ?kind { wd:Q196294 wd:Q1156659 }.
  }
  { ?floss wdt:P277 ?language .}
  OPTIONAL { ?floss wdt:P348 ?version .}
  OPTIONAL { ?floss wdt:P856 ?website .}
  OPTIONAL { ?floss wdt:P275 ?licence .}
  OPTIONAL { ?floss wdt:P306 ?os .}
} Limit 10 |]

escapedQuery :: String
escapedQuery = url ++ escapeURIString isAllowedInURI query


-- Datatype for a Software item
data Software = Software {
  floss :: !Text,
  language :: Maybe Text
  } deriving (Show, Generic)

data Collection = Collection [Software] deriving (Show, Generic)
data SPARQLResponse = SPARQLResponse Collection deriving (Show, Generic)

instance FromJSON Software where
  parseJSON (Object o) = 
    do 
      flossO <- o .: "floss"
      floss <- flossO .: "value"
      langO <- o .: "language" -- TODO: this has to be optional can be done with (.:?)
                               -- but I don't get the types right
      lang <- langO .: "value"
      return $ Software floss lang
  parseJSON _ = mzero

instance FromJSON Collection where
  parseJSON (Object o) =
    Collection <$> o .: "bindings"
  parseJSON _ = mzero

instance FromJSON SPARQLResponse where
  parseJSON (Object o) =
    SPARQLResponse <$> o .: "results"
  parseJSON _ = mzero


main :: IO ()
main = do
    print "foo"
    -- TODO Note: This function creates a new Manager. It should be avoided in production code.
    res <- eitherDecode <$> simpleHttp escapedQuery :: IO (Either String SPARQLResponse)
    case res of
      Left err -> putStrLn err
      Right ps -> print res
