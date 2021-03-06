{-# LANGUAGE   QuasiQuotes
             , OverloadedStrings
             , DataKinds
             , TypeFamilies
             , TypeOperators
             , GADTs #-}


module Floss.Query(
    getResource,
    query,
    queryLicense,
    queryCodings,
    queryOs,
    queryGui,
    queryCat,
    queryDev
) where


import Data.Aeson

import Network.URI(escapeURIString,isAllowedInURI)
import Network.HTTP.Client
import Network.HTTP.Client.TLS()

import Floss.Str(str)
import Floss.Parser

url :: String
url = "https://query.wikidata.org/sparql?format=json&query="

-- SPARQL query to get FLOSS from WikiData
query :: String
query = [str|
SELECT DISTINCT ?floss ?description ?name
                ?language ?license ?os ?cat ?gui ?dev
                ?version ?website ?logo ?img ?start ?repo ?fsd
WHERE {
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
  OPTIONAL { ?floss wdt:P348 ?version .}
  OPTIONAL { ?floss wdt:P856 ?website .}
  OPTIONAL { ?floss wdt:P154 ?logo .}
  OPTIONAL { ?floss wdt:P18  ?img .}
  OPTIONAL { ?floss wdt:P571 ?start .}
  OPTIONAL { ?floss wdt:P1324 ?repo .}
  OPTIONAL { ?floss wdt:P2537 ?fsd .}

  OPTIONAL { ?floss wdt:P275 ?license .}
  OPTIONAL { ?floss wdt:P306 ?os .}
  OPTIONAL { ?floss wdt:P277 ?language .}
  OPTIONAL { ?floss wdt:P1414 ?gui .}
  OPTIONAL { ?floss wdt:P178 ?dev .}

  OPTIONAL { 
  ?floss wdt:P31 ?cat.
  }

  OPTIONAL { ?floss rdfs:label ?name filter (lang(?name) = "en") .}
  OPTIONAL { ?floss schema:description ?description filter (lang(?description) = "en") .}
} |]

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

queryOs :: String
queryOs = [str|
SELECT DISTINCT ?os ?osLabel WHERE {
  ?free_software wdt:P306 ?os.
  SERVICE wikibase:label { bd:serviceParam wikibase:language "en". }
} |]

queryGui :: String
queryGui = [str|
SELECT DISTINCT ?gui ?guiLabel WHERE {
  ?free_software wdt:P1414 ?gui.
  SERVICE wikibase:label { bd:serviceParam wikibase:language "en". }
} |]

queryDev :: String
queryDev = [str|
SELECT DISTINCT ?dev ?devLabel WHERE {
  ?free_software wdt:P178 ?dev.
  SERVICE wikibase:label { bd:serviceParam wikibase:language "en". }
} |]

queryCat :: String
queryCat = [str|
SELECT DISTINCT ?category ?categoryLabel WHERE {
  {
   ?floss p:P31/ps:P31/wdt:P279* wd:Q506883.
   ?floss wdt:P31 ?category.
  } Union {
   ?floss p:P31/ps:P31/wdt:P279* wd:Q341.
   ?floss wdt:P31 ?category.
  } Union {
   ?floss p:P31/ps:P31/wdt:P279* wd:Q1130645.
   ?floss wdt:P31 ?category.
  } Union {
   ?floss wdt:P31 ?category.
   ?floss p:P31/ps:P31/wdt:P279* wd:Q19652.
   ?floss p:P31/ps:P31/wdt:P279* wd:Q7397.
  } Union {
   ?floss wdt:P31 ?category.
    ?floss p:P31/ps:P31/wdt:P279* wd:Q7397.
    ?floss wdt:P275 ?licens.
    ?licens p:P31/ps:P31/(wdt:P31|wdt:P279)* ?kind.
    VALUES ?kind { wd:Q196294 wd:Q1156659 }.
  }

  OPTIONAL { ?category rdfs:label ?categoryLabel filter (lang(?categoryLabel) = "en") .}
} |]

escapeQuery :: String -> String
escapeQuery = (url ++) . escapeURIString isAllowedInURI

getResource :: String -> Manager -> IO ItemList
getResource queryStr man = do
    req <- parseRequest $ escapeQuery queryStr
    res <- httpLbs req man
    let body   = responseBody res
        mbList = decode body :: Maybe SPARQLResponse
    case mbList of
        (Just (SPARQLResponse c)) -> return c
        _                         -> return Empty
