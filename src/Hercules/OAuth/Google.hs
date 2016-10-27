{-# LANGUAGE OverloadedStrings #-}

module Hercules.OAuth.Google
  ( googleAuthURL
  , googleOAuthSettings
  ) where

import Data.Monoid              ((<>))
import Data.Text
import Data.Text.Encoding
import Network.OAuth.OAuth2
import Network.Wai.Handler.Warp (Port)

import Hercules.Config (AuthInfo (..), HostName)
import Hercules.OAuth

googleOAuthSettings :: HostName -> Port -> AuthInfo -> OAuth2
googleOAuthSettings host port (AuthInfo clientID secret) = OAuth2
  { oauthClientId = clientID
  , oauthClientSecret = secret
  , oauthCallback = Just . encodeUtf8 $
      "http://" <> host <> ":" <> showT port <> "/google-auth"
  , oauthOAuthorizeEndpoint = "https://accounts.google.com/o/oauth2/auth"
  , oauthAccessTokenEndpoint = "https://www.googleapis.com/oauth2/v3/token"
  }

showT :: Show a => a -> Text
showT = pack . show

-- From hoauth2 test-google
-- | This is special for google Gain read-only access to the user's email
-- address.
googleScopeEmail :: QueryParams
googleScopeEmail = [("scope", "https://www.googleapis.com/auth/userinfo.email")]

googleAuthURL :: AuthState -> OAuth2 -> URI
googleAuthURL state googleAuthSettings =
  let queryParams = googleScopeEmail ++
        [("state", encodeUtf8 (unAuthState state))]
  in authorizationUrl googleAuthSettings `appendQueryParam` queryParams

