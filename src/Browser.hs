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


import           Floss.DB

import           Text.Hamlet
import           Text.Julius
import           Text.Lucius
import           Yesod                        hiding ((==.),check)

import qualified Data.Char                    as C
import qualified Data.Text                    as T
import           Data.Text
import           Data.List
import           Database.Esqueleto
import qualified Database.Persist             as P
import qualified Database.Persist.Sqlite      as P

import           Data.Foldable                as F
import           Control.Monad.Logger         (runStderrLoggingT)
import           Data.Maybe
import           System.Environment

newtype Browser = Browser ConnectionPool

instance Yesod Browser

mkYesod
  "Browser"
  [parseRoutes|
    /                          HomeR         GET
    /software/#String          SoftwareR     GET
    /softwarebyid/#Int         SoftwareIdR   GET

    /favicon.ico               FaviconR      GET

    /bylicense/#String         ByLicenseR    GET
    /bycoding/#String          ByCodingR     GET

    !/#String                   Filter1R     GET
    !/#String/#String           Filter2R     GET
    !/#String/#String/#String   Filter3R     GET
|]

instance YesodPersist Browser where
    type YesodPersistBackend Browser = SqlBackend
    runDB action = do
        Browser pool <- getYesod
        runSqlPool action pool

-- Simple Header providing a Home-Link
header :: Widget
header = do
    toWidget
      [lucius|
          .header {
              text-align: center
          }
      |]
    toWidget
      [hamlet|
        <div class="header">
          <a href=@{HomeR}>Home
      |]

-- TODO: move to proper place
normalizestr :: Text -> Text
normalizestr "" = ""
normalizestr t  = cons (C.toUpper $ T.head t) (T.tail t)

http2https :: Text -> Text
http2https = Data.Text.replace "http://" "https://"

-- Generate a nice Title for the page
gentitle :: String -> String -> String -> String
gentitle o l c = "Flossbrowser: Software" ++
    F.concat (Data.List.zipWith (+++) texts [o, l, c])
        where
          (+++) :: String -> String -> String
          (+++) _ "*" = ""
          (+++) a  b  = (++) a b
          texts = [" for ", " licenced under ", " written in "]

-- Query to get the list of all licenses
-- TODO: Cache results?
licenselist :: HandlerT Browser IO [String]
licenselist = do
  ll <- runDB
    $ select $ distinct
    $ from $ \(pl `InnerJoin` l) -> do
         on $ l ^. LicenseId ==. pl ^. ProjectLicenseFkLicenseId
         orderBy [ asc (l ^. LicenseName) ]
         return (l ^. LicenseName)
  return $ catMaybes $ fmap (fmap unpack . unValue ) ll

---- TODO: Cache results?
oslist :: HandlerT Browser IO [String]
oslist = do
  ol <- runDB
    $ select $ distinct
    $ from $ \(po `InnerJoin` o) -> do
         on $ o ^. OsId ==. po ^. ProjectOsFkOsId
         orderBy [ asc (o ^. OsName) ]
         return (o ^. OsName)
  return $ catMaybes $ fmap (fmap unpack . unValue ) ol

---- TODO: Cache results?
codinglist :: HandlerT Browser IO [String]
codinglist = do 
  cl <- runDB
    $ select $ distinct
    $ from $ \(pc `InnerJoin` c) -> do
         on $ c ^. CodingId ==. pc ^. ProjectCodingFkCodingId
         orderBy [ asc (c ^. CodingName) ]
         return (c ^. CodingName)
  return $ catMaybes $ fmap (fmap unpack . unValue ) cl

-- Chooser, to allow filtering for License, etc.
-- For now it works via Page-Redirect and the Recource-Handler do the work
-- The list of what options are available is currently given as an argument
chooser :: String -> String -> String -> Widget
chooser os license coding = do
    ll <- handlerToWidget licenselist
    cl <- handlerToWidget codinglist
    ol <- handlerToWidget oslist
    toWidget $(hamletFile "./templates/chooser.hamlet")
    toWidget $(juliusFile "./templates/chooser.julius")
    toWidget $(luciusFile "./templates/chooser.lucius")

runquery
  :: (YesodPersist site, YesodPersistBackend site ~ SqlBackend) =>
     Maybe String -> Maybe String -> Maybe String
      -> HandlerT site IO [Entity Project]
runquery os license coding = runDB
    $ select $ distinct
    $ from $ \p -> do
         case os of
           Just os' ->
             where_ $ p ^. ProjectId `in_`
               subList_select ( distinct $ from $
                 \(o `InnerJoin` po) -> do
                   on $ o ^. OsId       ==. po ^. ProjectOsFkOsId
                   where_ $ o ^. OsName ==. val (Just $ pack os') 
                   return $ po ^. ProjectOsFkProjectId
                   )
           Nothing -> return ()
         case license of
           Just license' ->
             where_ $ p ^. ProjectId `in_`
               subList_select ( distinct $ from $
                 \(l `InnerJoin` pl) -> do
                   on $ l ^. LicenseId      ==. pl ^. ProjectLicenseFkLicenseId
                   where_ $ l ^. LicenseName ==. val (Just $ pack license') 
                   return $ pl ^. ProjectLicenseFkProjectId
                   )
           Nothing -> return ()
         case coding of
           Just coding' ->
             where_ $ p ^. ProjectId `in_`
               subList_select ( distinct $ from $
                 \(c `InnerJoin` pl) -> do
                   on $ c ^. CodingId      ==. pl ^. ProjectCodingFkCodingId
                   where_ $ c ^. CodingName ==. val (Just $ pack coding') 
                   return $ pl ^. ProjectCodingFkProjectId
                   )
           Nothing -> return ()
         limit 50
         return p


-- List all Software
getHomeR :: Handler Html
getHomeR = getFilterN "*" "*" "*"

softwareWidget :: Key Project -> WidgetT Browser IO ()
softwareWidget key = do
    projects <- handlerToWidget $ runDB
           $ select $ distinct
           $ from $ \p -> do
                where_ ( p ^. ProjectId ==. val key )
                limit 1
                return p
    results <- handlerToWidget $ runDB
           $ select $ distinct
           $ from $ \(p `InnerJoin` pl `InnerJoin` l `InnerJoin` pc `InnerJoin` c) -> do
                on $ p ^. ProjectId ==. pl ^. ProjectLicenseFkProjectId
                on $ p ^. ProjectId ==. pc ^. ProjectCodingFkProjectId
                on $ l ^. LicenseId ==. pl ^. ProjectLicenseFkLicenseId
                on $ c ^. CodingId ==. pc ^. ProjectCodingFkCodingId
                where_ ( p ^. ProjectId ==. val key )
                return (l, c)
    let software = "Q" ++ show (fromSqlKey key)
    setTitle $ toHtml $ "Flossbrowser: " ++ software
    toWidget $(whamletFile "./templates/software.hamlet")
    toWidget $(luciusFile "./templates/software.lucius")

-- Show Details to one specified Software
getSoftwareR :: String -> Handler Html
getSoftwareR software = do
    projects <- runDB $ P.selectList [ ProjectName P.==. (Just $ pack software) ]  []
    defaultLayout $ mconcat <$> traverse ( softwareWidget . entityKey ) projects

-- Show Details to one specified Software
getSoftwareIdR :: Int -> Handler Html
getSoftwareIdR = defaultLayout . softwareWidget . qidtokey

getFilterN :: String -> String -> String -> Handler Html
getFilterN os license coding = do
    results <- runquery (check os) (check license) (check coding)
    defaultLayout $ do
      setTitle $ toHtml $ gentitle os license coding
      toWidget $(whamletFile "./templates/softwarelist.hamlet")
      toWidget $(luciusFile "./templates/softwarelist.lucius")
    where
      check "*" = Nothing
      check s   = Just s

-- Get Software by OS
getFilter1R :: String -> Handler Html
getFilter1R os = getFilterN os "*" "*"

-- Get Software by OS & License
getFilter2R :: String -> String -> Handler Html
getFilter2R os license = getFilterN os license "*"

-- Get Software by OS, License & Coding
getFilter3R :: String -> String -> String -> Handler Html
getFilter3R = getFilterN

-- Get Software by License-Name
getByLicenseR :: String -> Handler Html
getByLicenseR license = getFilterN "*" license "*"

-- Get Software my Coding-Name
getByCodingR :: String -> Handler Html
getByCodingR = getFilterN "*" "*"

getFaviconR :: Handler ()
getFaviconR = sendFile "image/vnd.microsoft.icon" "favicon.ico"

main :: IO ()
main = do
    t <- lookupEnv "PORT"
    let port = fromMaybe 3000 $ toint <$> t
    runStderrLoggingT $ P.withSqlitePool sqliteDBro 10 $
         \pool -> liftIO $ warp port $ Browser pool
      where
        toint s = read s :: Int
