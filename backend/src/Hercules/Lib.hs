{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Hercules.Lib
  ( startApp
  ) where

import Control.Monad.Log
import Data.Bifunctor                       (second)
import Data.Foldable                        (toList)
import Data.List                            (sortOn)
import Data.Maybe                           (catMaybes)
import Data.Monoid                          ((<>))
import Data.Text
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import Safe                                 (headMay)
import Servant
import Servant.Auth.Server                  (AuthResult (..),
                                             defaultCookieSettings)
import Servant.Mandatory

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.IO       as T

import Hercules.API
import Hercules.Config
import Hercules.Database.Extra       (JobsetNullable, Project,
                                      ProjectWithJobsets (..),
                                      fromNullableJobset, projectName)
import Hercules.OAuth
import Hercules.OAuth.Authenticators
import Hercules.OAuth.User
import Hercules.Query
import Hercules.ServerEnv
import Hercules.Static

startApp :: Config -> IO ()
startApp config = do
  let authenticators = configAuthenticatorList config
      port = configPort config
      logging = loggingMiddleware config
  env <- newEnv config authenticators
  T.putStrLn $ "Serving on http://" <> configHostname config
               <> ":" <> (pack . show $ port)
  run port . logging =<< app env

loggingMiddleware :: Config -> Middleware
loggingMiddleware config = case configAccessLogLevel config of
  Disabled    -> id
  Enabled     -> logStdout
  Development -> logStdoutDev

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
                :<|> (mandatory1 .∵ authCallback)
                :<|> loggedInPage
        queryApi = unprotected :<|> protected
        unprotected = getProjectNames
                      :<|> getProjects
                      :<|> getProject
                      :<|> getProjectsWithJobsets
        protected = getUser

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)

(.∵) :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
(.∵) = (.) . (.) . (.)

getUser :: AuthResult User -> App Text
getUser = \case
  (Authenticated (User (Email email))) -> pure email
  _                                    -> do
    logNotice "Failed user authentication attempt"
    throwError err401

getProjectNames :: App [Text]
getProjectNames = runQueryWithConnection projectNameQuery

getProject :: Text -> App (Maybe Project)
getProject name = headMay <$> runQueryWithConnection (projectQuery name)

getProjects :: App [Project]
getProjects = runQueryWithConnection projectsQuery

getProjectsWithJobsets :: App [ProjectWithJobsets]
getProjectsWithJobsets =
  fmap (uncurry makeProjectWithJobsets . second toList)
  . groupSortOn projectName
  <$> (runQueryWithConnection projectsWithJobsetsQuery :: App [(Project, JobsetNullable)])
  where
    makeProjectWithJobsets :: Project -> [JobsetNullable] -> ProjectWithJobsets
    makeProjectWithJobsets p jms =
      let js = catMaybes (fromNullableJobset <$> jms)
      in ProjectWithJobsets p js

groupSortOn :: Ord k => (a -> k) -> [(a, v)] -> [(a, NE.NonEmpty v)]
groupSortOn f = fmap (\x -> (fst $ NE.head x, fmap snd x))
          . NE.groupWith (f . fst)
          . sortOn (f . fst)

