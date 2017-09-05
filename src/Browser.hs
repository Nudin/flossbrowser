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
import           Yesod                        hiding ((==.))

import           Data.Text
import           Data.List
import           Database.Esqueleto
import qualified Database.Persist             as P
import qualified Database.Persist.Sqlite      as P
import           Database.Persist.TH

import           Data.Foldable                as F
import           Data.Monoid
import           Control.Monad
import           Control.Monad.Logger         (runStderrLoggingT)
import           Control.Monad.Trans.Resource (runResourceT)
import           Data.Maybe
import           System.Environment

data Browser = Browser ConnectionPool

instance Yesod Browser

mkYesod
  "Browser"
  [parseRoutes|
    /                          HomeR        GET
    /software/#String          SoftwareR    GET
    /softwarebyid/#Int         SoftwareIdR   GET

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

-- Generate a nice Title for the page
gentitle :: String -> String -> String -> String
gentitle o l c = "Flossbrowser: Software" ++
    F.concat (Data.List.zipWith (+++) texts [o, l, c])
        where
          (+++) :: String -> String -> String
          (+++) _ "*" = ""
          (+++) a b = (++) a b
          texts = [" for ", " licenced under ", " written in "]

-- Query to get the list of all licenses
-- TODO: Cache results?
licenselist :: HandlerT Browser IO [Entity License]
licenselist = runDB
    $ select $ distinct
    $ from $ \(pl `InnerJoin` l) -> do
         on $ l ^. LicenseId ==. pl ^. ProjectLicenseFkLicenseId
         orderBy [ asc (l ^. LicenseName) ]
         return l

---- TODO: Cache results?
oslist :: HandlerT Browser IO [Entity Os]
oslist = runDB
    $ select $ distinct
    $ from $ \(po `InnerJoin` o) -> do
         on $ o ^. OsId ==. po ^. ProjectOsFkOsId
         orderBy [ asc (o ^. OsName) ]
         return o

---- TODO: Cache results?
codinglist :: HandlerT Browser IO [Entity Coding]
codinglist = runDB
    $ select $ distinct
    $ from $ \(pc `InnerJoin` c) -> do
         on $ c ^. CodingId ==. pc ^. ProjectCodingFkCodingId
         orderBy [ asc (c ^. CodingName) ]
         return c

-- Chooser, to allow filtering for License, etc.
-- For now it works via Page-Redirect and the Recource-Handler do the work
-- The list of what options are available is currently given as an argument
chooser :: String -> String -> String -> Widget
chooser os license coding = do
    ll <- handlerToWidget $ licenselist
    cl <- handlerToWidget $ codinglist
    ol <- handlerToWidget $ oslist
    toWidget $(hamletFile "./templates/chooser.hamlet")
    toWidget $(juliusFile "./templates/chooser.julius")

runquery
  :: (YesodPersist site, YesodPersistBackend site ~ SqlBackend) =>
     Maybe String -> Maybe String -> Maybe String
      -> HandlerT site IO [Entity Project]
runquery os license coding = runDB
    $ select $ distinct
    $ from $ \(p `InnerJoin` pl `InnerJoin` l `InnerJoin` pc
              `InnerJoin` c `InnerJoin` o `InnerJoin` po) -> do
         on $ p ^. ProjectId ==. pl ^. ProjectLicenseFkProjectId
         on $ p ^. ProjectId ==. pc ^. ProjectCodingFkProjectId
         on $ p ^. ProjectId ==. po ^. ProjectOsFkProjectId
         on $ l ^. LicenseId ==. pl ^. ProjectLicenseFkLicenseId
         on $ c ^. CodingId  ==. pc ^. ProjectCodingFkCodingId
         on $ o ^. OsId      ==. po ^. ProjectOsFkOsId
         case os of
           Just os' -> where_ ( o ^. OsName ==. val (Just (pack os')))
           Nothing -> return ()
         case license of
           Just license' -> where_ ( l ^. LicenseName ==. val (Just (pack license')))
           Nothing -> return ()
         case coding of
           Just coding' -> where_ ( c ^. CodingName ==. val (Just (pack coding')))
           Nothing -> return ()
         limit 50
         return p


-- List all Software
getHomeR :: Handler Html
--getHomeR = getFilterN "*" "*" "*" -- elegant but slow version
getHomeR = do
    results <- runDB $ P.selectList []  [P.LimitTo 50]
    defaultLayout $ do
       setTitle "Floss-Browser"
       let os = "*"
       let coding = "*"
       let license = "*"
       toWidget $(whamletFile "./templates/softwarelist.hamlet")
       toWidget $(luciusFile "./templates/softwarelist.lucius")

-- TODO: Maybe give key instead of qid? 
softwareWidget qid = do
    projects <- handlerToWidget $ runDB
           $ select $ distinct
           $ from $ \p -> do
                where_ ( p ^. ProjectId ==. val (qidtokey qid) )
                limit 1
                return p
    results <- handlerToWidget $ runDB
           $ select $ distinct
           $ from $ \(p `InnerJoin` pl `InnerJoin` l `InnerJoin` pc `InnerJoin` c) -> do
                on $ p ^. ProjectId ==. pl ^. ProjectLicenseFkProjectId
                on $ p ^. ProjectId ==. pc ^. ProjectCodingFkProjectId
                on $ l ^. LicenseId ==. pl ^. ProjectLicenseFkLicenseId
                on $ c ^. CodingId ==. pc ^. ProjectCodingFkCodingId
                where_ ( p ^. ProjectId ==. val (qidtokey qid) )
                return (l, c)
    let software = "Q" ++ (show qid)
    setTitle $ toHtml $ "Flossbrowser: " ++ software
    toWidget $(whamletFile "./templates/software.hamlet")
    toWidget $(luciusFile "./templates/software.lucius")

-- Show Details to one specified Software
getSoftwareR :: String -> Handler Html
getSoftwareR software = do
    projects <- runDB $ P.selectList [ ProjectName P.==. (Just $ pack software) ]  []
    defaultLayout $ fmap mconcat $ sequence $ -- TODO: is there a more direct way?
      fmap ( softwareWidget . fromIntegral . fromSqlKey . entityKey ) projects

-- Show Details to one specified Software
getSoftwareIdR :: Int -> Handler Html
getSoftwareIdR = defaultLayout . softwareWidget

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
getByCodingR coding = getFilterN "*" "*" coding

main :: IO ()
main = do
    t <- lookupEnv "PORT"
    let port = fromMaybe 3000 $ toint <$> t
    runStderrLoggingT $ P.withSqlitePool sqliteDB 10 $
         \pool -> liftIO $ warp port $ Browser pool
      where
        toint s = read s :: Int
