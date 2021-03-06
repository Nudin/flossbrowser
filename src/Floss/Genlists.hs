{-# LANGUAGE   TemplateHaskell
             , EmptyDataDecls
             , FlexibleContexts
             , GADTs
             , GeneralizedNewtypeDeriving
             , MultiParamTypeClasses
             , OverloadedStrings
             , QuasiQuotes
             , TypeFamilies
             , ViewPatterns #-}

module Floss.Genlists where

import Language.Haskell.TH
import Database.Esqueleto
import Data.Char

import Floss.DB
import Floss.Str

genlist :: String -> ExpQ
genlist s = code
  where
    tableId      = mkName $ s ++ "Id"
    crosstableId = mkName $ "Project" ++ s ++ "XId"
    tableName    = mkName $ s ++ "Name"
    code = [| do
      ol <- runDB $ select $ distinct
        $ from $ \(pl `InnerJoin` l) -> do
             on $ l ^. $(conE tableId) ==. pl ^. $(conE crosstableId)
             orderBy [ asc (l ^. $(conE tableName)) ]
             return (l ^. $(conE tableName))
      return $ sort $ fmap normalizestr $ catMaybes $ fmap unValue ol
      |]

queryXTable :: String -> ExpQ
queryXTable s = code
  where
    tableId      = mkName $ s ++ "Id"
    crosstableId = mkName $ "Project" ++ s ++ "XId"
    crosstablePId = mkName $ "Project" ++ s ++ "PId"
    tableName    = mkName $ s ++ "Name"
    code = [| runDB
           $ select $ distinct
           $ from $ \(po `InnerJoin` o) -> do
                on $ o ^. $(conE tableId)      ==. po ^. $(conE crosstableId)
                where_ ( po ^. $(conE crosstablePId) ==. val key)
                return (o ^. $(conE tableName))
            |]

gencheck :: String -> ExpQ
gencheck t = code
  where
    tableId       = mkName $ t ++ "Id"
    crosstableId  = mkName $ "Project" ++ t ++ "XId"
    crosstablePId = mkName $ "Project" ++ t ++ "PId"
    tableName     = mkName $ t ++ "Name"
    var           = mkName $ fmap toLower t
    code = [|
      case $(varE var) f of
           Just _filter ->
             where_ $ p ^. ProjectId `in_`
               subList_select ( distinct $ from $
                 \(o `InnerJoin` po) -> do
                   on     $ o  ^. $(conE tableId)   ==. po ^. $(conE crosstableId)
                   where_ $ lower_ (o  ^. $(conE tableName)) ==. lower_ ( val (Just _filter) )
                   return $ po ^. $(conE crosstablePId)
                   )
           Nothing -> return ()
           |]
