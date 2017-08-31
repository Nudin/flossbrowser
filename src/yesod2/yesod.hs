{-# LANGUAGE  OverloadedStrings, TypeFamilies, ViewPatterns, TemplateHaskell, QuasiQuotes #-}

module Main where

import Yesod
import Text.Hamlet
import Text.Lucius

data MyApp = MyApp
instance Yesod MyApp

mkYesod "MyApp" [parseRoutes|
  /                 HomeR     GET
  /software/#Text   SoftwareR GET
  /foo              FooR      GET
|]


getHomeR :: Handler Html
getHomeR = defaultLayout $ do 
  setTitle "Floss-Browser"  
  toWidget [whamlet|
      <h1>Hello Yesod!1!
      Some text that 
      is <i>displayed</i> here.
      
      <p> This is a link to 
        <a href=@{FooR}>foo 
  |]
  
getSoftwareR :: Handler Html
getSoftwareR software = defaultLayout $ do 
  setTitle software
  toWidget $(luciusFile "./foo.lucius")
  toWidget $(hamletFile "./foo.hamlet")

getFooR :: Handler Html
getFooR = defaultLayout $ do 
  setTitle "Foo"
  toWidget $(luciusFile "./foo.lucius")
  toWidget $(hamletFile "./foo.hamlet")
  
  
--getStudentsR :: Int -> Handler Html
--getStudentsR i = defaultLayout $ do
--  setTitle "Students"
--  toWidget [whamlet|
--    <h1>Students enlisted:
--    <ul>
--      <li> #{show $ ffpStudents !! i}  
--  |]
    
    
main :: IO ()
main = warp 3000 MyApp


