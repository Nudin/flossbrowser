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

import           Data.Foldable                as F
import           Control.Monad.Logger         (runStderrLoggingT)
import           Control.Monad.Reader
import           Data.Maybe

data Browser = Browser ConnectionPool

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

    !/#String                                   Filter1R     GET
    !/#String/#String                           Filter2R     GET
    !/#String/#String/#String                   Filter3R     GET
    !/#String/#String/#String/#String           Filter4R     GET
    !/#String/#String/#String/#String/#String   Filter5R     GET
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


-- Chooser, to allow filtering for License, etc.
-- For now it works via Page-Redirect and the Recource-Handler do the work
-- The list of what options are available is currently given as an argument
chooser :: String -> String -> String -> String -> String -> Widget
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
    Maybe String -> Maybe String -> Maybe String -> Maybe String -> Maybe String
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
getSoftwareR :: String -> Handler Html
getSoftwareR software = do
    projects <- runDB $ P.selectList [ ProjectName P.==. (Just $ pack software) ]  []
    defaultLayout $ mconcat <$> traverse ( softwareWidget . entityKey ) projects

-- Show Details to one specified Software
getSoftwareIdR :: Int -> Handler Html
getSoftwareIdR = defaultLayout . softwareWidget . qidtokey

getFilterN :: String -> String -> String -> String -> String -> Handler Html
getFilterN cat os license coding gui = do
    results <- runquery (check cat) (check os) (check license) (check coding) (check gui)
    defaultLayout $ do
      setTitle $ toHtml $ gentitle os license coding
      toWidget $(whamletFile "./templates/softwarelist.hamlet")
      toWidget $(luciusFile "./templates/softwarelist.lucius")
    where
      check "*" = Nothing
      check s   = Just s

-- Get Software by Cat
getFilter1R :: String -> Handler Html
getFilter1R cat = getFilterN cat "*" "*" "*" "*"

-- Get Software by Cat & OS
getFilter2R :: String -> String -> Handler Html
getFilter2R cat os = getFilterN cat os "*" "*" "*"

-- Get Software by Cat, OS & License
getFilter3R :: String -> String -> String -> Handler Html
getFilter3R cat os license = getFilterN cat os license "*" "*"

-- Get Software by Cat, OS & License
getFilter4R :: String -> String -> String -> String -> Handler Html
getFilter4R cat os license coding = getFilterN cat os license coding "*"

-- Get Software by OS, License & Coding
getFilter5R :: String -> String -> String -> String -> String -> Handler Html
getFilter5R = getFilterN

-- Get Software by License-Name
getByLicenseR :: String -> Handler Html
getByLicenseR license = getFilterN "*" "*" license "*" "*"

-- Get Software my Coding-Name
getByCodingR :: String -> Handler Html
getByCodingR coding = getFilterN "*" "*" "*" coding "*" 

-- Get Software my Gui-Name
getByGuiR :: String -> Handler Html
getByGuiR = getFilterN "*" "*" "*" "*" 

getFaviconR :: Handler ()
getFaviconR = sendFile "image/vnd.microsoft.icon" "favicon.ico"

newtype BrowserEnv = BrowserEnv { port :: Int }

-- This application runs in a reader / IO transformer stack
type BrowserT m = ReaderT BrowserEnv m
type BrowserIO  = BrowserT IO

runBrowserT :: Monad m => BrowserEnv -> BrowserT m a -> m a
runBrowserT = flip runReaderT

server :: BrowserIO ()
server = do
    e <- ask
    let p = port e
    liftIO $ runStderrLoggingT $ P.withSqlitePool sqliteDBro 10 $
             \pool -> liftIO $ warp p $ Browser pool
    return ()

main :: IO ()
main = do
    let be = BrowserEnv { port = 3000 }
    runBrowserT be server
