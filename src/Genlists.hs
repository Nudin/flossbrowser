{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

module Genlists where

import           Language.Haskell.TH
import           Database.Esqueleto
import qualified Database.Persist             as P
import qualified Database.Persist.Sqlite      as P

import Floss.DB

genlist s = code
  where
    tableId = mkName (s++"Id")
    crosstableId = mkName ("Project"++s++(Prelude.head s):"Id") -- TODO: head
    tableName = mkName (s++"Name")
    code = [| do
      ol <- runDB $ select $ distinct
        $ from $ \(pl `InnerJoin` l) -> do
             on $ l ^. $(conE tableId) ==. pl ^. $(conE crosstableId)
             orderBy [ asc (l ^. $(conE tableName)) ]
             return (l ^. $(conE tableName))
      return $ catMaybes $ fmap (fmap unpack . unValue ) ol
      |]
