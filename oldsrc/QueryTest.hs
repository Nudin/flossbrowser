{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy.Char8 as L8

import Network.URI(escapeURIString,isAllowedInURI)
import Network.HTTP.Client
import Network.HTTP.Client.TLS

import Str(str)

url :: String
url = "https://query.wikidata.org/sparql?query="

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
  OPTIONAL { ?floss wdt:P277 ?language .}
  OPTIONAL { ?floss wdt:P348 ?version .}
  OPTIONAL { ?floss wdt:P856 ?website .}
  OPTIONAL { ?floss wdt:P275 ?licence .}
  OPTIONAL { ?floss wdt:P306 ?os .}
} |]

escapedQuery :: String
escapedQuery = url ++ escapeURIString isAllowedInURI query

main :: IO ()
main = do
    manager <- newManager tlsManagerSettings
    req <- parseUrl escapedQuery
    res <- httpLbs req manager
    print $ responseBody res
