{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Hercules.Lib
  ( startApp
  ) where

import Data.Text
import Network.Wai
import Network.Wai.Handler.Warp
import Safe                     (headMay)
import Servant
import Servant.Auth.Server      (AuthResult (..), defaultCookieSettings,
                                 defaultJWTSettings, generateKey, makeJWT)
import Servant.Mandatory

import Hercules.API
import Hercules.Config
import Hercules.Database  (Project)
import Hercules.OAuth
import Hercules.Query
import Hercules.ServerEnv
import Hercules.Static

startApp :: Config -> IO ()
startApp config = do
  env <- newEnv config
  run (configPort config) =<< app env

app :: Env -> IO Application
app env = do
  key <- generateKey
  let api = Proxy :: Proxy API
      jwtConfig = defaultJWTSettings key
      authConfig = defaultCookieSettings :. jwtConfig :. EmptyContext
  print =<< makeJWT (User "joe") jwtConfig Nothing
  pure $ serveWithContext api authConfig (server env)

server :: Env -> Server API
server env = enter (Nat (runApp env)) api
  where api = queryApi
              :<|> pages
        pages = welcomePage
                :<|> loginPage
                :<|> (mandatory2 . authPage)
        queryApi = unprotected :<|> protected
        unprotected = getProjectNames
                      :<|> getProject
        protected = getUser


getUser :: AuthResult User -> App Text
getUser = \case
  (Authenticated (User name)) -> pure name
  _                           -> throwError err401

getProjectNames :: App [Text]
getProjectNames = runQueryWithConnection projectNameQuery

getProject :: Text -> App (Maybe Project)
getProject name = headMay <$> runQueryWithConnection (projectQuery name)
