{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

{-|
GitHub specific OAuth2 functionality
-}
module Hercules.OAuth.Authenticators.GitHub
  ( githubAuthenticator
  ) where

import Data.Aeson.TH
import Data.Maybe           (fromJust)
import Data.Text            hiding (tail)
import Network.HTTP.Client  (Manager)
import Network.OAuth.OAuth2 hiding (URI)
import Network.URI

import Hercules.Config            (AuthClientInfo (..))
import Hercules.Database.Hercules
import Hercules.OAuth.Types
import Hercules.OAuth.User
import Hercules.Query.Hercules
import Hercules.ServerEnv

{-# ANN module ("HLint: Ignore Use CamelCase" :: String) #-}


data GitHubUser =
  GitHubUser { gid    :: Integer
             , gname  :: Text
             , gemail :: Text
             }
  deriving (Show, Eq)

deriveJSON defaultOptions{fieldLabelModifier = tail} ''GitHubUser

githubAuthenticator
  :: (AuthenticatorName -> URI)
  -> AuthClientInfo
  -> OAuth2Authenticator App
githubAuthenticator makeCallback clientInfo =
  makeAuthenticator makeCallback
                    (AuthenticatorName "github")
                    githubScopeEmail
                    githubOAuthEndpoint
                    githubAccessTokenEndpoint
                    clientInfo
                    githubGetUserInfo

githubScopeEmail :: QueryParams
githubScopeEmail = [("scope", "user:email")]

githubOAuthEndpoint :: OAuthEndpoint
githubOAuthEndpoint = OAuthEndpoint . fromJust . parseURI
                    $ "https://github.com/login/oauth/authorize"

githubAccessTokenEndpoint :: AccessTokenEndpoint
githubAccessTokenEndpoint = AccessTokenEndpoint . fromJust . parseURI
                          $ "https://github.com/login/oauth/access_token"

githubGetUserInfo :: AccessToken -> App (Either Text UserId)
githubGetUserInfo token = do
  withHttpManager (\m -> getUserInfo m token) >>= \case
    Left _err  -> pure $ Left "Error getting user info"
    Right user -> findOrCreateUser user

findOrCreateUser :: GitHubUser -> App (Either Text UserId)
findOrCreateUser user = do
  let textId = pack . show . gid $ user
  runHerculesQueryWithConnection (userIdQuery textId) >>= \case
    []  -> createUser user
    [u] -> pure $ Right (UserId (userId (u :: User)))
    _   -> pure $ Left "Multiple users with the same id in database!"

createUser :: GitHubUser -> App (Either Text UserId)
createUser GitHubUser{..} = do
  let user = User () gname gemail (pack . show $ gid)
  withHerculesConnection (\c -> insertUser c user) >>= \case
    Nothing -> pure $ Left "Error inserting user"
    Just i -> pure $ Right i

getUserInfo :: Manager -> AccessToken -> IO (OAuth2Result GitHubUser)
getUserInfo manager token = do
  authGetJSON manager token "https://api.github.com/user"
