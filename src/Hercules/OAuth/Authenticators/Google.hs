{-# LANGUAGE OverloadedStrings #-}

module Hercules.OAuth.Authenticators.Google
  ( googleAuthenticator
  ) where

import Data.Maybe           (fromJust)
import Network.OAuth.OAuth2 hiding (URI)
import Network.URI

import Hercules.Config      (AuthClientInfo (..))
import Hercules.OAuth.Types

googleAuthenticator
  :: (AuthenticatorName -> URI)
  -> AuthClientInfo
  -> OAuth2Authenticator
googleAuthenticator makeCallback =
  makeAuthenticator makeCallback
                    (AuthenticatorName "google")
                    googleScopeEmail
                    googleOAuthEndpoint
                    googleAccessTokenEndpoint

googleOAuthEndpoint :: OAuthEndpoint
googleOAuthEndpoint = OAuthEndpoint . fromJust . parseURI $ "https://accounts.google.com/o/oauth2/auth"

googleAccessTokenEndpoint :: AccessTokenEndpoint
googleAccessTokenEndpoint = AccessTokenEndpoint . fromJust . parseURI $ "https://www.googleapis.com/oauth2/v3/token"

-- | The scope parameter for the users email address
googleScopeEmail :: QueryParams
googleScopeEmail = [("scope", "https://www.googleapis.com/auth/userinfo.email")]
