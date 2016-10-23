{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Lib
  ( startApp
  ) where

import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant

import API

app :: Application
app = serve (Proxy :: Proxy API) server

server :: Server API
server = pure "Hello"

startApp :: IO ()
startApp = run 8080 app

