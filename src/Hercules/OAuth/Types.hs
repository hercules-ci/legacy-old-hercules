{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StrictData                 #-}

{-|
A module describing some newtypes and data types to handle information used
during authentication.

Several of these types are just newtypes around URI or Bytestring just to make
passing them round a little safer.
-}
module Hercules.OAuth.Types
  ( AuthClientInfo(..)
  , OAuth2Authenticator
  , makeAuthenticator
  , authenticatorName
  , authenticatorConfig
  , authenticatorAuthQueryParams
  -- * newtypes
  , AuthenticatorName(..)
  , AuthState(..)
  , AuthClientState(..)
  , AuthCode(..)
  , UserAuthURL(..)
  , OAuthEndpoint(..)
  , AccessTokenEndpoint(..)
  ) where

import Data.ByteString
import Data.Text
import Network.OAuth.OAuth2 hiding (URI)
import Network.URI.Extra
import Servant              (FromHttpApiData)

-- | The name of an authenticator, "google" or "github" for example
newtype AuthenticatorName = AuthenticatorName
  { unAuthenticatorName :: Text }
  deriving (Eq, FromHttpApiData)

-- | The state the client can pass when authenticating, this will be returned
-- if the authentication is successful.
newtype AuthClientState = AuthClientState
  { unAuthClientState :: Text }
  deriving (FromHttpApiData)

-- | This is the value which is actually sent to the authenticator. It's a
-- combination of the users data and some unique state generated on the server
-- which is checked upon completion of the authentication to prevent CSRF
-- attacks.
newtype AuthState = AuthState
  { unAuthState :: Text }
  deriving (FromHttpApiData)

newtype AuthCode = AuthCode
  { unAuthCode :: Text }
  deriving (FromHttpApiData)

newtype OAuthEndpoint = OAuthEndpoint
  { unOAuthEndpoint :: URI }

newtype AccessTokenEndpoint = AccessTokenEndpoint
  { unAccessTokenEndpoint :: URI}

-- | A URL to redirect the user to in order for them to authenticate themselves
newtype UserAuthURL = UserAuthURL
  { unUserAuthURL :: ByteString }

-- | The Id and secret for a client on an external service
data AuthClientInfo = AuthClientInfo
  { authClientInfoId     :: ByteString
  , authClientInfoSecret :: ByteString
  }
  deriving(Read, Show)


-- | A collection of all the information necessary to authenticate with a
-- provider
--
-- One should use 'makeAuthenticator' to construct values of this type as the
-- config and name must remain in sync.
data OAuth2Authenticator = OAuth2Authenticator
  { authenticatorName            :: AuthenticatorName
  , authenticatorConfig          :: OAuth2
  , authenticatorAuthQueryParams :: QueryParams
  }

-- | Construct an 'OAuth2Authenticator'
makeAuthenticator
  :: (AuthenticatorName -> URI)
  -- ^ A function to make the callback URI for the named authenticator
  -> AuthenticatorName
  -- ^ The name of this authenticator
  -> QueryParams
  -- ^ Any additional query params this authenticator requires
  -> OAuthEndpoint
  -> AccessTokenEndpoint
  -> AuthClientInfo
  -> OAuth2Authenticator
makeAuthenticator makeCallback name queryParams
                  authEndpoint accessTokenEndpoint AuthClientInfo{..} =
    OAuth2Authenticator { authenticatorName = name
                        , authenticatorConfig = config
                        , authenticatorAuthQueryParams = queryParams
                        }
  where
    config :: OAuth2
    config = OAuth2
      { oauthClientId = authClientInfoId
      , oauthClientSecret = authClientInfoSecret
      , oauthCallback = Just . uriToByteString . makeCallback $ name
      , oauthOAuthorizeEndpoint =
          uriToByteString . unOAuthEndpoint $ authEndpoint
      , oauthAccessTokenEndpoint =
          uriToByteString . unAccessTokenEndpoint $ accessTokenEndpoint
      }

