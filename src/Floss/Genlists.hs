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

genlist :: String -> ExpQ
genlist s = code
  where
    tableId      = mkName $ s ++ "Id"
    crosstableId = mkName $ "Project" ++ s ++ Prelude.head s:"Id" -- TODO: head
    tableName    = mkName $ s ++ "Name"
    code = [| do
      ol <- runDB $ select $ distinct
        $ from $ \(pl `InnerJoin` l) -> do
             on $ l ^. $(conE tableId) ==. pl ^. $(conE crosstableId)
             orderBy [ asc (l ^. $(conE tableName)) ]
             return (l ^. $(conE tableName))
      return $ catMaybes $ fmap unValue ol
      |]

gencheck :: String -> ExpQ
gencheck t = code
  where
    tableId       = mkName $ t ++ "Id"
    crosstableId  = mkName $ "Project" ++ t ++ Prelude.head t:"Id" -- TODO: head
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
                   where_ $ o  ^. $(conE tableName) ==. val (Just _filter)
                   return $ po ^. $(conE crosstablePId)
                   )
           Nothing -> return ()
           |]
