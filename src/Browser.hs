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
import Genlists

import           Text.Hamlet
import           Text.Julius
import           Text.Lucius
import           Yesod                        hiding ((==.), check)

import qualified Data.Char                    as C
import qualified Data.Text                    as T
import           Data.Text
import           Data.List
import           Database.Esqueleto
import qualified Database.Persist             as P
import qualified Database.Persist.Sqlite      as P

import           Control.Monad.Logger         (runStderrLoggingT)
import           Control.Monad.Reader
import           Data.Maybe
import           Data.Configurator            as Conf
import           Data.Configurator.Types      as Conf

data Browser = Browser ConnectionPool

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

-- Generate a nice Title for the page
gentitle :: Text -> Text -> Text -> Text
gentitle o l c = "Flossbrowser: Software" `Data.Text.append`
    Data.Text.concat (Data.List.zipWith (+++) texts [o, l, c])
        where
          (+++) :: Text -> Text -> Text
          (+++) _ "*" = ""
          (+++) a  b  = Data.Text.append a b
          texts = [" for ", " licenced under ", " written in "]


-- Chooser, to allow filtering for License, etc.
-- For now it works via Page-Redirect and the Recource-Handler do the work
-- The list of what options are available is currently given as an argument
chooser :: Text -> Text -> Text -> Text -> Text -> Widget
chooser cat os license coding gui = do
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
    Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text
      -> HandlerT site IO [Entity Project]
runquery cat os license coding gui = runDB
    $ select $ distinct
    $ from $ \p -> do
         $(gencheck "os"      "Os")
         $(gencheck "cat"     "Cat")
         $(gencheck "license" "License")
         $(gencheck "coding"  "Coding")
         $(gencheck "gui"     "Gui")
         limit 50
         return p


-- List all Software
getHomeR :: Handler Html
getHomeR = getFilterN "*" "*" "*" "*" "*"

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

-- Show Details to one specified Software
getSoftwareR :: Text -> Handler Html
getSoftwareR software = do
    projects <- runDB $ P.selectList [ ProjectName P.==. (Just software) ]  []
    defaultLayout $ mconcat <$> traverse ( softwareWidget . entityKey ) projects

-- Show Details to one specified Software
getSoftwareIdR :: Int -> Handler Html
getSoftwareIdR = defaultLayout . softwareWidget . qidtokey

getFilterN :: Text -> Text -> Text -> Text -> Text -> Handler Html
getFilterN cat os license coding gui = do
    results <- runquery (check cat) (check os) (check license) (check coding) (check gui)
    defaultLayout $ do
      setTitle $ toHtml $ gentitle os license coding
      toWidget $(whamletFile "./templates/softwarelist.hamlet")
      toWidget $(luciusFile "./templates/softwarelist.lucius")
    where
      check "*" = Nothing
      check s   = Just s

getFilterR :: [Text] -> Handler Html
getFilterR (cat:os:license:coding:gui:[]) = getFilterN cat os license coding gui
getFilterR _ = notFound

-- Get Software by License-Name
getByLicenseR :: Text -> Handler Html
getByLicenseR license = getFilterN "*" "*" license "*" "*"

-- Get Software my Coding-Name
getByCodingR :: Text -> Handler Html
getByCodingR coding = getFilterN "*" "*" "*" coding "*" 

-- Get Software my Gui-Name
getByGuiR :: Text -> Handler Html
getByGuiR = getFilterN "*" "*" "*" "*" 

getFaviconR :: Handler ()
getFaviconR = sendFile "image/vnd.microsoft.icon" "favicon.ico"

newtype BrowserEnv = BrowserEnv { port :: Int }

type BrowserT m = ReaderT BrowserEnv m
type BrowserIO  = BrowserT IO

-- This application runs in a reader / IO transformer stack
runBrowserT :: Monad m => BrowserEnv -> BrowserT m a -> m a
runBrowserT = flip runReaderT

handleConfError :: Show t => t -> IO ()
handleConfError e = print $ "Error reading config file: " ++ show e

confSettings :: AutoConfig
confSettings  = AutoConfig { interval = 10
                           , onError  = handleConfError }

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
