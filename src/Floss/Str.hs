{-# LANGUAGE OverloadedStrings #-}

module Floss.Str( str
                , normalizestr
                , http2https
                , strtoid
                , urltoid
                )
where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Data.Text                    as T
import Data.Char                    as C

import Floss.Types

-- replace only quoteExp for multiline strings
-- qqUndef only defined to suppress warnings of uninitialized fields
qqUndef :: QuasiQuoter
qqUndef  = QuasiQuoter {quoteExp  = undefined ,
                        quotePat  = undefined ,
                        quoteType = undefined ,
                        quoteDec  = undefined }

str :: QuasiQuoter
str  = qqUndef { quoteExp = stringE }


normalizestr :: Text -> Text
normalizestr "" = ""
normalizestr t  = cons (C.toUpper $ T.head t) (T.tail t)
{-# INLINABLE normalizestr #-}

http2https :: Text -> Text
http2https = T.replace "http://" "https://"
{-# INLINABLE http2https #-}

-- Convert String to WikidataItemID
-- fallback -1
strtoid :: String -> WikidataItemID
strtoid = strtoid_ where
  strtoid_ :: String -> Int
  strtoid_ "" = -1
  strtoid_ s = read s
{-# INLINABLE strtoid #-}

-- Convert IRI of Wikidata-Item to an Int-ID
urltoid :: String -> WikidataItemID
urltoid = strtoid . Prelude.drop 32
{-# INLINABLE urltoid #-}
