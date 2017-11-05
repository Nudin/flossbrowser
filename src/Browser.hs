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
import           Floss.Config (readConfig)

import           Text.Hamlet
import           Text.Julius
import           Text.Lucius
import           Yesod                   hiding (check, (==.), (||.))
import           Yesod.Static

import           Data.List               as L
import           Data.Text
import           Database.Esqueleto
import qualified Database.Persist        as P

import           Control.Monad
import           Control.Monad.Logger    (runStderrLoggingT, filterLogger)
import           Control.Monad.Reader
import           Data.Configurator       as Conf
import           Data.Configurator.Types as Conf
import           Data.Maybe

staticFiles "static"

data Browser = Browser {
  myApproot      :: Text,
  connectionpool :: ConnectionPool,
  getStatic      :: Static
}

type BrowserT m = ReaderT FlossEnv m
type BrowserIO  = BrowserT IO

instance Yesod Browser where
  approot = ApprootMaster myApproot
  -- TODO: clean up this messy function
  cleanPath site s =
      if corrected == s'
           then Right $ L.map dropDash s'
           else Left corrected
      where
        s' = dropprefix s
        corrected = L.filter (not . Data.Text.null) s'
        dropDash t
            | Data.Text.all (== '-') t = Data.Text.drop 1 t
            | otherwise = t
        r = Data.Text.drop 1 $ myApproot site
        dropprefix l
            | L.take 1 l == [r] = dropprefix $ L.drop 1 l
            | l == [""] = []
            | otherwise = l

instance YesodPersist Browser where
    type YesodPersistBackend Browser = SqlBackend
    runDB action = do
        liftHandlerT $ addHeader "X-Accel-Expires" "3600"
        Browser _ pool _ <- getYesod
        runSqlPool action pool

mkYesod
  "Browser"
  [parseRoutes|
    /                        HomeR         GET
    /software/#Text          SoftwareR     GET
    /softwarebyid/#Int       SoftwareIdR   GET

    /static                  StaticR       Static getStatic
    /favicon.ico             FaviconR      GET

    /bylicense/#Text         ByLicenseR    GET
    /bycoding/#Text          ByCodingR     GET

    /search/#Text            SearchR       GET

    !/*Texts                 FilterR       GET
|]

-- Simple Header providing a Home-Link
header :: Widget
header = do
    toWidget
      [lucius|
          .header {
              text-align: center;
              height: 2em;
          }
      |]
    toWidget
      [hamlet|
        <div class="header">
          <a href=@{HomeR}>Home
      |]
footer :: Widget
footer = do
    toWidget
      [lucius|
          .footer {
              text-align: center;
              width: 100%;
              background-color: lightgrey;
              height: 2em;
              font-weight: 900;
          }
          .footer p {
              margin-top: auto;
              margin-bottom: auto;
          }
      |]
    toWidget
      [hamlet|
        <div class="footer">
          <p>Flossbrowser is still alpha! URLs will change! You can help!
            &nbsp;→&nbsp;<a href="https://gitlab.com/Nudin/flossbrowser">Code
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
    Data.Text.concat (L.zipWith (+++) texts f)
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
    addScript $ StaticR js_jquery_3_2_1_min_js
    addScript $ StaticR js_select2_min_js
    addStylesheet $ StaticR css_select2_min_css

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
    let wikidataid = fromSqlKey key
    maybeproject <- handlerToWidget $ runDB $ get key
    results <- handlerToWidget $ do
      t <- $(queryXTable "Cat")
      o <- $(queryXTable "Os")
      l <- $(queryXTable "License")
      c <- $(queryXTable "Coding")
      g <- $(queryXTable "Gui")
      d <- $(queryXTable "Dev")
      return (listall t, listall o, listall l, listall c, listall g, listall d)
    let title = case join $ fmap projectName maybeproject of
                  Just t -> unpack t
                  Nothing -> "Q" ++ show wikidataid
    setTitle $ toHtml $ "Flossbrowser: " ++ title
    toWidget $(whamletFile "./templates/software.hamlet")
    toWidget $(luciusFile "./templates/software.lucius")
    toWidget $(luciusFile "./templates/main.lucius")
    addStylesheet $ StaticR css_lightbox_min_css
    addScript $ StaticR js_lightbox_min_js
    toWidget [julius|
      var lightbox = new Lightbox();
      lightbox.load();
      |]
    where
        listall a = case sequence $ nub $ fmap unValue a of
                Just [] -> "Unknown or none!"
                Nothing -> "Unknown!"
                Just l  -> Data.Text.intercalate "; " l

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
        toWidget $(luciusFile "./templates/main.lucius")
    where
        check "*" = Nothing
        check s   = Just s

-- Show a list of software projects matching the searchtext
getSearchR :: Text -> Handler Html
getSearchR t = do
    let f = [Just "*", Just "*", Just "*", Just "*", Just "*"]
    let s = Just $ "%" `Data.Text.append` t `Data.Text.append` "%"
    results <- runDB
      $ select $ distinct
      $ from $ \p -> do
        where_ $
          ( p ^. ProjectDescription `like` val s ) ||.
          ( p ^. ProjectName `like` val s ) ||.
          ( p ^. ProjectId `in_` (
            subList_select $ from $ \pc -> do
              where_ $ pc ^. ProjectCatXId `in_` (
                subList_select $ from $ \c -> do
                  where_ $ c ^. CatName `like` val s
                  return $ c ^. CatId
                                                 )
              return $ pc ^. ProjectCatPId
            )
          )
        limit 50
        return p
    defaultLayout $ do
        setTitle $ toHtml $ "Search for" `Data.Text.append` t
        toWidget $(whamletFile "./templates/softwarelist.hamlet")
        toWidget $(luciusFile "./templates/softwarelist.lucius")
        toWidget $(luciusFile "./templates/main.lucius")

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
runBrowserT :: Monad m => FlossEnv -> BrowserT m a -> m a
runBrowserT = flip runReaderT

server :: BrowserIO ()
server = do
    env <- ask
    static@(Static _) <- liftIO $ static "static"
    let p = port env
    let r = root env
    let dbt = backend env
    liftIO $ runStderrLoggingT $ filterLogger (\_ lvl -> lvl /= LevelDebug )
           $ withDBPool dbt $ \pool -> liftIO $ warp p $ Browser r pool static
    return ()


main :: IO ()
main = do
    be <- readConfig
    runBrowserT be server
