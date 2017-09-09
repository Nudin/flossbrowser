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

import           Control.Monad
import           Control.Monad.Logger         (runStderrLoggingT)
import           Data.Maybe
import           System.Environment

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


-- List all Software
getHomeR :: Handler Html
getHomeR = getFilterR ["*", "*", "*", "*", "*"]

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

getFilterN :: SoftwareFilter -> Handler Html
getFilterN f = do
    results <- runquery f
    defaultLayout $ do
      setTitle $ toHtml $ gentitle f
      toWidget $(whamletFile "./templates/softwarelist.hamlet")
      toWidget $(luciusFile "./templates/softwarelist.lucius")

getFilterR :: [Text] -> Handler Html
getFilterR filter = getFilterN $ fmap check filter
    where
      check "*" = Nothing
      check s   = Just s

-- Get Software by License-Name
getByLicenseR :: Text -> Handler Html
getByLicenseR license = getFilterR ["*", "*", license, "*", "*"]

-- Get Software my Coding-Name
getByCodingR :: Text -> Handler Html
getByCodingR coding = getFilterR ["*", "*", "*", coding, "*"]

-- Get Software my Gui-Name
getByGuiR :: Text -> Handler Html
getByGuiR gui = getFilterR ["*", "*", "*", "*", gui]

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
