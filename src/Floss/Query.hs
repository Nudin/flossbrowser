{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}


module Floss.Query(
    getCollection,
    getLicenses,
    getCodings,
) where

import Str(str)

import Data.Aeson

import Network.URI(escapeURIString,isAllowedInURI)
import Network.HTTP.Client
import Network.HTTP.Client.TLS()

import Floss.Types

url :: String
url = "https://query.wikidata.org/sparql?format=json&query="

query :: String
query = [str|
SELECT DISTINCT ?floss ?name ?language ?version ?website ?license ?os WHERE {
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
  OPTIONAL { ?floss wdt:P275 ?license .}
  OPTIONAL { ?floss wdt:P306 ?os .}

  OPTIONAL { ?floss rdfs:label ?name filter (lang(?name) = "en") .}
} Limit 100 |]

queryLicense :: String
queryLicense = [str|
SELECT DISTINCT ?license ?licenseLabel WHERE {
  ?free_software wdt:P275 ?license.
  SERVICE wikibase:label { bd:serviceParam wikibase:language "en". }
} |]

queryCodings :: String
queryCodings = [str|
SELECT DISTINCT ?language ?languageLabel WHERE {
  ?free_software wdt:P277 ?language.
  SERVICE wikibase:label { bd:serviceParam wikibase:language "en". }
} |]

escapeQuery :: String -> String
escapeQuery = (url ++) . escapeURIString isAllowedInURI

getCollection :: Manager -> IO Collection
getCollection man = do
    req <- parseUrl $ escapeQuery query
    res <- httpLbs req man
    let body   = responseBody res
        mbList = decode body :: Maybe SPARQLResponse
    case mbList of
        (Just (SPARQLResponse c)) -> return c
        _                         -> return $ Collection []

getLicenses :: Manager -> IO LicenseList
getLicenses man = do
    req <- parseUrl $ escapeQuery queryLicense
    res <- httpLbs req man
    let body   = responseBody res
        mbList = decode body :: Maybe SPARQLResponse
    case mbList of
        (Just (SPARQLResponseLicenses c)) -> return c
        _                                 -> return $ LicenseList []

getCodings :: Manager -> IO CodingList
getCodings man = do
    req <- parseUrl $ escapeQuery queryCodings
    res <- httpLbs req man
    let body   = responseBody res
        mbList = decode body :: Maybe SPARQLResponse
    case mbList of
        (Just (SPARQLResponseCodings c)) -> return c
        _                                -> return $ CodingList []
