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
import Servant.Auth.Server      (AuthResult (..), defaultCookieSettings)
import Servant.Mandatory

import Hercules.API
import Hercules.Config
import Hercules.Database             (Project)
import Hercules.OAuth
import Hercules.OAuth.Authenticators
import Hercules.OAuth.User
import Hercules.Query
import Hercules.ServerEnv
import Hercules.Static

startApp :: Config -> IO ()
startApp config = do
  let authenticators = configAuthenticatorList config
  env <- newEnv config authenticators
  run (configPort config) =<< app env

app :: Env -> IO Application
app env = do
  let api = Proxy :: Proxy API
      authConfig = defaultCookieSettings :. envJWTSettings env :. EmptyContext
  pure $ serveWithContext api authConfig (server env)

server :: Env -> Server API
server env = enter (Nat (runApp env)) api
  where api = queryApi
              :<|> pages
        pages = welcomePage
                :<|> (mandatory1 .: loginPage)
                :<|> (mandatory1 .⋮ authCallback)
                :<|> loggedInPage
        queryApi = unprotected :<|> protected
        unprotected = getProjectNames
                      :<|> getProjects
                      :<|> getProject
        protected = getUser

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)

(.⋮) :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
(.⋮) = (.) . (.) . (.)

getUser :: AuthResult User -> App Text
getUser = \case
  (Authenticated (User (Email email))) -> pure email
  _                                    -> throwError err401

getProjectNames :: App [Text]
getProjectNames = runQueryWithConnection projectNameQuery

getProject :: Text -> App (Maybe Project)
getProject name = headMay <$> runQueryWithConnection (projectQuery name)

getProjects :: App [Project]
getProjects = runQueryWithConnection projectsQuery
