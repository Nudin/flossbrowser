{-# LANGUAGE   EmptyDataDecls
             , FlexibleContexts
             , GADTs
             , GeneralizedNewtypeDeriving
             , MultiParamTypeClasses
             , OverloadedStrings
             , QuasiQuotes
             , TemplateHaskell
             , TypeFamilies
             , ViewPatterns #-}

-- To avoid warning for unused yesod-generated resources
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

import           Floss.DB
import           Floss.Genlists
import           Floss.Str

import           Text.Hamlet
import           Text.Julius
import           Text.Lucius
import           Yesod                   hiding (check, (==.))

import           Data.List
import           Data.Text
import           Database.Esqueleto
import qualified Database.Persist        as P
import qualified Database.Persist.Sqlite as P

import           Control.Monad
import           Control.Monad.Logger    (runStderrLoggingT, filterLogger)
import           Control.Monad.Reader
import           Data.Configurator       as Conf
import           Data.Configurator.Types as Conf
import           Data.Maybe

data Browser = Browser {
  myApproot      :: Text,
  connectionpool :: ConnectionPool
}
data BrowserEnv = BrowserEnv {
  port :: Int,
  root :: Text
}

type BrowserT m = ReaderT BrowserEnv m
type BrowserIO  = BrowserT IO

instance Yesod Browser where
  approot = ApprootMaster myApproot
  -- TODO: clean up this messy function
  cleanPath site s = do
      if corrected == s'
           then Right $ Data.List.map dropDash s'
           else Left  $ corrected
      where
        s' = dropprefix s
        corrected = Data.List.filter (not . Data.Text.null) s'
        dropDash t
            | Data.Text.all (== '-') t = Data.Text.drop 1 t
            | otherwise = t
        r = Data.Text.drop 1 $ myApproot site
        dropprefix l
            | Data.List.take 1 l == [r] = dropprefix $ Data.List.drop 1 l
            | l == [""] = []
            | otherwise = l

instance YesodPersist Browser where
    type YesodPersistBackend Browser = SqlBackend
    runDB action = do
        Browser _ pool <- getYesod
        runSqlPool action pool

mkYesod
  "Browser"
  [parseRoutes|
    /                        HomeR         GET
    /software/#Text          SoftwareR     GET
    /softwarebyid/#Int       SoftwareIdR   GET

    /favicon.ico             FaviconR      GET

    /bylicense/#Text         ByLicenseR    GET
    /bycoding/#Text          ByCodingR     GET

    !/*Texts                 FilterR       GET
|]

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


-- Save version of !!, returning a maybe
(!!!) :: [a] -> Int -> Maybe a
(!!!) a i = if Prelude.length a>i then Just (a !! i) else Nothing

type SoftwareFilter =  [ Maybe Text ]

-- Getter functions for the SoftwareFilter
cat     :: SoftwareFilter -> Maybe Text
cat     f = join $ f !!! 0
os      :: SoftwareFilter -> Maybe Text
os      f = join $ f !!! 1
license :: SoftwareFilter -> Maybe Text
license f = join $ f !!! 2
coding  :: SoftwareFilter -> Maybe Text
coding  f = join $ f !!! 3
gui     :: SoftwareFilter -> Maybe Text
gui     f = join $ f !!! 4


-- Generate a nice Title for the page
gentitle :: SoftwareFilter -> Text
gentitle f = "Flossbrowser: Software" `Data.Text.append`
    Data.Text.concat (Data.List.zipWith (+++) texts f)
        where
          (+++) :: Text -> Maybe Text -> Text
          (+++) _ Nothing  = ""
          (+++) a (Just b) = Data.Text.append a b
          texts = [" of type ", " for ", " licenced under ", " written in ", " with "]


-- Chooser, to allow filtering for License, etc.
-- For now it works via Page-Redirect and the Recource-Handler does the work
-- The list of available options is currently given as an argument
chooser :: SoftwareFilter -> Widget
chooser f = do
    ll <- handlerToWidget $(genlist "License")
    cl <- handlerToWidget $(genlist "Coding")
    ol <- handlerToWidget $(genlist "Os")
    gl <- handlerToWidget $(genlist "Gui")
    tl <- handlerToWidget $(genlist "Cat")
    toWidget $(hamletFile "./templates/chooser.hamlet")
    toWidget $(juliusFile "./templates/chooser.julius")
    toWidget $(luciusFile "./templates/chooser.lucius")

-- Run a query against the database to find all softwares matching
-- the given filter-options, all filters are optional.
-- To avoid boilerplate code, we generate the sub queries with Template Haskell
runquery
  :: (YesodPersist site, YesodPersistBackend site ~ SqlBackend) =>
     SoftwareFilter -> HandlerT site IO [Entity Project]
runquery f = runDB
    $ select $ distinct
    $ from $ \p -> do
         $(gencheck "Os")
         $(gencheck "Cat")
         $(gencheck "License")
         $(gencheck "Coding")
         $(gencheck "Gui")
         limit 50
         return p

-- Widget showing all the informations about one software.
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
           $ from $ \(p `InnerJoin` pl `InnerJoin`
                      l `InnerJoin` pc `InnerJoin` c) -> do
                on $ p ^. ProjectId ==. pl ^. ProjectLicensePId
                on $ p ^. ProjectId ==. pc ^. ProjectCodingPId
                on $ l ^. LicenseId ==. pl ^. ProjectLicenseXId
                on $ c ^. CodingId  ==. pc ^. ProjectCodingXId
                where_ ( p ^. ProjectId ==. val key )
                return (l, c)
    let software = "Q" ++ show (fromSqlKey key)
    setTitle $ toHtml $ "Flossbrowser: " ++ software
    toWidget $(whamletFile "./templates/software.hamlet")
    toWidget $(luciusFile "./templates/software.lucius")

-- List all Software
getHomeR :: Handler Html
getHomeR = getFilterR ["*", "*", "*", "*", "*"]

-- Show details to a specified Software by name
-- Note: There can be more than one software with the exact same name,
-- in this case we concatenate the widgets of all matches
getSoftwareR :: Text -> Handler Html
getSoftwareR software = do
    projects <- runDB $ P.selectList [ ProjectName P.==. Just software ]  []
    defaultLayout $ mconcat <$> traverse ( softwareWidget . entityKey ) projects

-- Show Details to one specified Software by Wikidata-ID
getSoftwareIdR :: Int -> Handler Html
getSoftwareIdR = defaultLayout . softwareWidget . qidtokey

-- Show a list of software projects matching the chosen criteria
getFilterR :: Texts -> Handler Html
getFilterR t = do
    let f = fmap check t
    results <- runquery f
    defaultLayout $ do
        setTitle $ toHtml $ gentitle f
        toWidget $(whamletFile "./templates/softwarelist.hamlet")
        toWidget $(luciusFile "./templates/softwarelist.lucius")
    where
        check "*" = Nothing
        check s   = Just s

-- Get Software by License-Name
getByLicenseR :: Text -> Handler Html
getByLicenseR l = getFilterR ["*", "*", l, "*", "*"]

-- Get Software by Coding-Name
getByCodingR :: Text -> Handler Html
getByCodingR c = getFilterR ["*", "*", "*", c, "*"]

-- Get Software by Gui-Name
getByGuiR :: Text -> Handler Html
getByGuiR ui = getFilterR ["*", "*", "*", "*", ui]

getFaviconR :: Handler ()
getFaviconR = sendFile "image/vnd.microsoft.icon" "favicon.ico"


-- This application runs in a reader / IO transformer stack
runBrowserT :: Monad m => BrowserEnv -> BrowserT m a -> m a
runBrowserT = flip runReaderT

handleConfError :: Show t => t -> IO ()
handleConfError e = print $ "Error reading config file: " ++ show e

confSettings :: AutoConfig
confSettings  = AutoConfig { interval = 10,
                             onError  = handleConfError }

readConfig :: IO BrowserEnv
readConfig = do
    (conf, _) <- autoReload confSettings [Required "./flossrc"]
    mbPort <- Conf.lookup conf "port" :: IO (Maybe Int)
    let p = case mbPort of
                (Just v) -> v
                Nothing  -> 3000
    mbRoot <- Conf.lookup conf "root" :: IO (Maybe Text)
    let r = case mbRoot of
                (Just v) -> v
                Nothing  -> ""
    return BrowserEnv { port = p, root = r }

server :: BrowserIO ()
server = do
    env <- ask
    let p = port env
    let r = root env
    liftIO $ runStderrLoggingT $ filterLogger (\_ lvl -> lvl /= LevelDebug )
           $ P.withSqlitePool sqliteDBro 100
           $ \pool -> liftIO $ warp p $ Browser r pool
    return ()


main :: IO ()
main = do
    be <- readConfig
    runBrowserT be server
