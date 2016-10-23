{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module Hercules.Lib
  ( startApp
  ) where

import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import Hercules.API
import Hercules.Config

app :: Application
app = serve (Proxy :: Proxy API) server

server :: Server API
server = pure "Hello"

startApp :: Config -> IO ()
startApp Config{..} = run configPort app

