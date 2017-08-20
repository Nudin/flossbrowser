{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}


module Floss.Query(
    getCollection,
) where

import Str(str)
import Control.Monad

import Data.Aeson

import Network.URI(escapeURIString,isAllowedInURI)
import Network.HTTP.Client
import Network.HTTP.Client.TLS

import Floss.Types

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

getCollection :: IO Collection
getCollection = do
    manager <- newManager tlsManagerSettings
    req <- parseUrl escapedQuery
    res <- httpLbs req manager
    let body   = responseBody res
        mbList = decode body :: Maybe SPARQLResponse
    case mbList of
        (Just (SPARQLResponse c)) -> return c
        _                         -> return $ Collection []
