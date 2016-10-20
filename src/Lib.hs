{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Lib
  ( startApp
  ) where

import           Data.Text
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant

type API = Get '[JSON] Text

app :: Application
app = serve (Proxy :: Proxy API) server

server :: Server API
server = pure "Hello"

startApp :: IO ()
startApp = run 8080 app

