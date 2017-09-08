{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

module Genlists where

import Language.Haskell.TH
import Database.Esqueleto

import Floss.DB

genlist :: String -> ExpQ
genlist s = code
  where
    tableId      = mkName (s ++ "Id")
    crosstableId = mkName ("Project" ++ s ++ Prelude.head s:"Id") -- TODO: head
    tableName    = mkName (s ++ "Name")
    code = [| do
      ol <- runDB $ select $ distinct
        $ from $ \(pl `InnerJoin` l) -> do
             on $ l ^. $(conE tableId) ==. pl ^. $(conE crosstableId)
             orderBy [ asc (l ^. $(conE tableName)) ]
             return (l ^. $(conE tableName))
      return $ catMaybes $ fmap (fmap unpack . unValue ) ol
      |]

gencheck :: String -> String -> ExpQ
gencheck s t = code
  where
    tableId       = mkName (t ++ "Id")
    crosstableId  = mkName ("Project" ++ t ++ Prelude.head t:"Id") -- TODO: head
    crosstablePId = mkName ("Project" ++ t ++ "PId") -- TODO: head
    tableName     = mkName (t ++ "Name")
    var           = mkName s
    code = [|
        case $(varE var) of
           Just _filter ->
             where_ $ p ^. ProjectId `in_`
               subList_select ( distinct $ from $
                 \(o `InnerJoin` po) -> do
                   on     $ o  ^. $(conE tableId)   ==. po ^. $(conE crosstableId)
                   where_ $ o  ^. $(conE tableName) ==. val (Just $ pack _filter)
                   return $ po ^. $(conE crosstablePId)
                   )
           Nothing -> return ()
           |]
