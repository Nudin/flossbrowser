{-# LANGUAGE ScopedTypeVariables #-}

import Floss.FillDB
import Control.Exception

import Network.HTTP.Client

main :: IO ()
main = initDB `catch` (\(e :: HttpException) -> handleHTTPEx e)

handleHTTPEx :: HttpException -> IO ()
handleHTTPEx _ = print "A network error occurred"
