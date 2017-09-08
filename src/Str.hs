module Str(str) where
 
import Language.Haskell.TH
import Language.Haskell.TH.Quote

-- replace only quoteExp for multiline strings
-- qqUndef only defined to suppress warnings of uninitialized fields
qqUndef :: QuasiQuoter
qqUndef = QuasiQuoter {quoteExp = undefined ,
                       quotePat = undefined ,
                       quoteType = undefined,
                       quoteDec = undefined }

str :: QuasiQuoter
str = qqUndef { quoteExp = stringE }
