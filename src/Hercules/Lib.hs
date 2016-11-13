{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Hercules.Lib
  ( startApp
  ) where

import Data.Bifunctor           (second)
import Data.Foldable            (toList)
import Data.Int
import Data.List                (sortOn)
import Data.Maybe               (catMaybes)
import Data.Text
import Network.Wai
import Network.Wai.Handler.Warp
import Safe                     (headMay)
import Servant
import Servant.Auth.Server      (AuthResult (..), defaultCookieSettings)
import Servant.Mandatory

import qualified Data.List.NonEmpty as NE

import Hercules.API
import Hercules.Config
import Hercules.Database.Extra       (Jobset, Jobset' (..), Project,
                                      ProjectWithJobsets (..), projectName)
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
                      :<|> getProjectsWithJobsets
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

getProjectsWithJobsets :: App [ProjectWithJobsets]
getProjectsWithJobsets =
  fmap (uncurry makeProjectWithJobsets . second toList)
  . groupSortOn projectName
  <$> (runQueryWithConnection projectsWithJobsetsQuery :: App [(Project, JobsetM)])
  where
    makeProjectWithJobsets :: Project -> [JobsetM] -> ProjectWithJobsets
    makeProjectWithJobsets p jms =
      let js = catMaybes (sequenceMaybeJobset <$> jms)
      in ProjectWithJobsets p js

groupSortOn :: Ord k => (a -> k) -> [(a, v)] -> [(a, NE.NonEmpty v)]
groupSortOn f = fmap (\x -> (fst $ NE.head x, fmap snd x))
          . NE.groupWith (f . fst)
          . sortOn (f . fst)

type JobsetM = Jobset' (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Int32) (Maybe Int32) (Maybe Int32) (Maybe Int32) (Maybe Int32) (Maybe Int32) (Maybe Text) (Maybe Int32) (Maybe Int32) (Maybe Int32) (Maybe Text)

-- TODO: derive this with some TH
sequenceMaybeJobset :: JobsetM -> Maybe Jobset
sequenceMaybeJobset (Jobset c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17) =
  Jobset
  <$> c1
  <*> c2
  <*> pure c3
  <*> c4
  <*> c5
  <*> pure c6
  <*> pure c7
  <*> pure c8
  <*> pure c9
  <*> c10
  <*> c11
  <*> c12
  <*> c13
  <*> c14
  <*> c15
  <*> c16
  <*> pure c17
