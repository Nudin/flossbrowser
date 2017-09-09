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

import           Text.Hamlet
import           Text.Julius
import           Text.Lucius
import           Yesod                   hiding (check, (==.))

import qualified Data.Char               as C
import           Data.List
import           Data.Text
import qualified Data.Text               as T
import           Database.Esqueleto
import qualified Database.Persist        as P
import qualified Database.Persist.Sqlite as P

import           Control.Monad
import           Control.Monad.Logger    (runStderrLoggingT)
import           Control.Monad.Reader
import           Data.Configurator       as Conf
import           Data.Configurator.Types as Conf
import           Data.Maybe

newtype Browser    = Browser ConnectionPool
newtype BrowserEnv = BrowserEnv { port :: Int }

type BrowserT m = ReaderT BrowserEnv m
type BrowserIO  = BrowserT IO

instance Yesod Browser

mkYesod
  "Browser"
  [parseRoutes|
    /                          HomeR         GET
    /software/#Text          SoftwareR     GET
    /softwarebyid/#Int         SoftwareIdR   GET

    /favicon.ico               FaviconR      GET

    /bylicense/#Text         ByLicenseR    GET
    /bycoding/#Text          ByCodingR     GET

    !/*Texts                  FilterR     GET
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


(!!!) :: [a] -> Int -> Maybe a
(!!!) a i = if Prelude.length a>i then Just (a !! i) else Nothing

type SoftwareFilter =  [ Maybe Text ]

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
-- For now it works via Page-Redirect and the Recource-Handler do the work
-- The list of what options are available is currently given as an argument
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
                on $ l ^. LicenseId ==. pl ^. ProjectLicenseLId
                on $ c ^. CodingId ==. pc ^. ProjectCodingCId
                where_ ( p ^. ProjectId ==. val key )
                return (l, c)
    let software = "Q" ++ show (fromSqlKey key)
    setTitle $ toHtml $ "Flossbrowser: " ++ software
    toWidget $(whamletFile "./templates/software.hamlet")
    toWidget $(luciusFile "./templates/software.lucius")

-- List all Software
getHomeR :: Handler Html
getHomeR = getFilterR ["*", "*", "*", "*", "*"]

-- Show Details to one specified Software
getSoftwareR :: Text -> Handler Html
getSoftwareR software = do
    projects <- runDB $ P.selectList [ ProjectName P.==. Just software ]  []
    defaultLayout $ mconcat <$> traverse ( softwareWidget . entityKey ) projects

-- Show Details to one specified Software
getSoftwareIdR :: Int -> Handler Html
getSoftwareIdR = defaultLayout . softwareWidget . qidtokey

getFilterR :: Texts -> Handler Html
getFilterR f' = do
    let f = fmap check f'
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

-- Get Software my Coding-Name
getByCodingR :: Text -> Handler Html
getByCodingR c = getFilterR ["*", "*", "*", c, "*"]

-- Get Software my Gui-Name
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
    return BrowserEnv { port = p }

server :: BrowserIO ()
server = do
    env <- ask
    let p = port env
    liftIO $ runStderrLoggingT $ P.withSqlitePool sqliteDBro 10 $
             \pool -> liftIO $ warp p $ Browser pool
    return ()


main :: IO ()
main = do
    be <- readConfig
    runBrowserT be server
