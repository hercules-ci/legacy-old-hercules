{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
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
  , authenticatorGetUserInfo
  , AuthState(..)
  -- * newtypes
  , AuthenticatorName(..)
  , AuthClientState(..)
  , AuthCode(..)
  , AuthError(..)
  , AuthStatePacked(..)
  , UserAuthURL(..)
  , OAuthEndpoint(..)
  , AccessTokenEndpoint(..)
  , FrontendURL(..)
  , PackedJWT(..)
  ) where

import Data.Aeson
import Data.ByteString.Char8
import Data.Text             (Text)
import GHC.Generics          (Generic)
import Network.OAuth.OAuth2  hiding (URI)
import Network.URI.Extra
import Servant               (FromHttpApiData)

import Hercules.OAuth.User

-- | The name of an authenticator, "google" or "github" for example
newtype AuthenticatorName = AuthenticatorName
  { unAuthenticatorName :: Text }
  deriving (Eq, FromHttpApiData)

-- | The state the client can pass when authenticating, this will be returned
-- if the authentication is successful.
newtype AuthClientState = AuthClientState
  { unAuthClientState :: Text }
  deriving (ToJSON, FromJSON, Show, FromHttpApiData)

-- | This is the value which is actually sent to the authenticator. It's a
-- combination of the users data and some unique state generated on the server
-- which is checked upon completion of the authentication to prevent CSRF
-- attacks.
data AuthState = AuthState
  { authStateFrontendURL :: FrontendURL
  , authStateClientState :: Maybe AuthClientState
  }
  deriving (Generic, Show)

instance ToJSON AuthState where
instance FromJSON AuthState where

newtype AuthStatePacked = AuthStatePacked
  { unAuthStatePacked :: Text }
  deriving (FromHttpApiData, Show)

newtype FrontendURL = FrontendURL
  { unFrontendURL :: Text }
  deriving (ToJSON, FromJSON, Show, FromHttpApiData)

newtype AuthCode = AuthCode
  { unAuthCode :: Text }
  deriving (FromHttpApiData)

newtype AuthError = AuthError
  { unAuthError :: Text }
  deriving (FromHttpApiData)

newtype OAuthEndpoint = OAuthEndpoint
  { unOAuthEndpoint :: URI }

newtype AccessTokenEndpoint = AccessTokenEndpoint
  { unAccessTokenEndpoint :: URI}

-- | A URL to redirect the user to in order for them to authenticate themselves
newtype UserAuthURL = UserAuthURL
  { unUserAuthURL :: ByteString }

newtype PackedJWT = PackedJWT
  { unPackedJWT :: ByteString }

-- | The Id and secret for a client on an external service
data AuthClientInfo = AuthClientInfo
  { authClientInfoId     :: ByteString
  , authClientInfoSecret :: ByteString
  }
  deriving(Read, Show)

instance FromJSON AuthClientInfo where
  parseJSON = withObject "AuthClientInfo" (\v ->
    AuthClientInfo <$> (pack <$> v .: "id")
                   <*> (pack <$> v .: "secret"))

-- | A collection of all the information necessary to authenticate with a
-- provider
--
-- One should use 'makeAuthenticator' to construct values of this type as the
-- config and name must remain in sync.
--
-- The authentication validation takes place in the monad 'm'
data OAuth2Authenticator m = OAuth2Authenticator
  { authenticatorName            :: AuthenticatorName
  , authenticatorConfig          :: OAuth2
  , authenticatorAuthQueryParams :: QueryParams
  , authenticatorGetUserInfo     :: AccessToken -> m (Either Text UserId)
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
  -> (AccessToken -> m (Either Text UserId))
  -> OAuth2Authenticator m
makeAuthenticator makeCallback name queryParams
                  authEndpoint accessTokenEndpoint AuthClientInfo{..}
                  getUserInfo =
    OAuth2Authenticator { authenticatorName = name
                        , authenticatorConfig = config
                        , authenticatorAuthQueryParams = queryParams
                        , authenticatorGetUserInfo = getUserInfo
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

