{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-|
A module providing data types and functions for getting information about
different authentication providers.
-}
module Hercules.OAuth.Authenticators
  ( AuthenticatorName(..)
  , OAuth2Authenticator
  , configAuthenticatorList
  , authenticationURLWithState
  ) where

import Data.Aeson
import Data.ByteString      (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Maybe           (catMaybes)
import Data.Text
import Network.OAuth.OAuth2 hiding (URI)
import Network.URI          (URI (..), URIAuth (..))

import Hercules.Config
import Hercules.OAuth.Authenticators.Google
import Hercules.OAuth.Types
import Hercules.ServerEnv                   (App)

-- | Get a list of usable authentication services from the given config.
configAuthenticatorList :: Config -> [OAuth2Authenticator App]
configAuthenticatorList Config{..} = catMaybes
  [ googleAuthenticator makeCallback <$> configGoogleAuthInfo ]
  where
    makeCallback :: AuthenticatorName -> URI
    makeCallback (AuthenticatorName name) =
      let authority = URIAuth "" (unpack configHostName) (":" ++ show configPort)
          path = "/auth-callback/" ++ unpack name
      in URI "http:" (Just authority) path "" ""

-- | Get the URL to redirect clients to with the given state to roundtrip
authenticationURLWithState :: OAuth2Authenticator m -> AuthState -> UserAuthURL
authenticationURLWithState authenticator state =
  let stateBS = packAuthState state
      queryParams = authenticatorAuthQueryParams authenticator
                    ++ [("state", stateBS)]
      config = authenticatorConfig authenticator
  in UserAuthURL $ authorizationUrl config `appendQueryParam` queryParams

packAuthState :: AuthState -> ByteString
packAuthState = toStrict . encode
