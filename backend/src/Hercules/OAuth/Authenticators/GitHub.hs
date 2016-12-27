{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
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

import Hercules.Config      (AuthClientInfo (..))
import Hercules.OAuth.Types
import Hercules.OAuth.User
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
    Left _err -> pure $ Left "Error getting user info"
    Right user -> pure $ Right (UserId 0) -- TODO: Fix

getUserInfo :: Manager -> AccessToken -> IO (OAuth2Result GitHubUser)
getUserInfo manager token = do
  authGetJSON manager token "https://api.github.com/user"

