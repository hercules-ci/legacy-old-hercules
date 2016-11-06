{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

{-|
Google specific OAuth2 functionality
-}
module Hercules.OAuth.Authenticators.Google
  ( googleAuthenticator
  ) where

import Control.Concurrent.Async (concurrently)
import Data.Aeson.TH
import Data.Maybe               (fromJust)
import Data.Text
import Data.Text.Encoding
import Network.HTTP.Client      (Manager)
import Network.OAuth.OAuth2     hiding (URI)
import Network.URI

import Hercules.Config      (AuthClientInfo (..))
import Hercules.OAuth.Types
import Hercules.OAuth.User
import Hercules.ServerEnv

{-# ANN module ("HLint: Ignore Use CamelCase" :: String) #-}

data GoogleToken = GoogleToken
  { audience   :: Text
  , scope      :: Text
  , userid     :: Maybe Text
  , expires_in :: Integer
  }
  deriving (Show)

deriveJSON defaultOptions ''GoogleToken

data GoogleUser = GoogleUser
  { id             :: Text
  , email          :: Text
  , verified_email :: Bool
  , name           :: Text
  }
  deriving (Show)

deriveJSON defaultOptions ''GoogleUser

googleAuthenticator
  :: (AuthenticatorName -> URI)
  -> AuthClientInfo
  -> OAuth2Authenticator App
googleAuthenticator makeCallback clientInfo=
  makeAuthenticator makeCallback
                    (AuthenticatorName "google")
                    googleScopeEmail
                    googleOAuthEndpoint
                    googleAccessTokenEndpoint
                    clientInfo
                    (googleGetUserInfo clientInfo)

googleOAuthEndpoint :: OAuthEndpoint
googleOAuthEndpoint = OAuthEndpoint . fromJust . parseURI $ "https://accounts.google.com/o/oauth2/auth"

googleAccessTokenEndpoint :: AccessTokenEndpoint
googleAccessTokenEndpoint = AccessTokenEndpoint . fromJust . parseURI $ "https://www.googleapis.com/oauth2/v3/token"

-- | The scope parameter for the users email address
googleScopeEmail :: QueryParams
googleScopeEmail = [("scope", "https://www.googleapis.com/auth/userinfo.email")]

googleGetUserInfo :: AuthClientInfo -> AccessToken -> App (Either Text User)
googleGetUserInfo clientInfo token = do
  (tokenInfo', userInfo') <-
    withHttpManager (\m -> concurrently (validateToken m token)
                                        (getUserInfo m token))

  let ourClientId = decodeUtf8 $ authClientInfoId clientInfo

  case tokenInfo' of
    Left _err -> pure $ Left "Error getting token info"
    Right tokenInfo -> case userInfo' of
      Left _err -> pure $ Left "Error getting user info"
      Right userInfo -> pure $
        -- Check that the token is valid and matches our client. This is important to
        -- avoid the "confused deputy issue".
        if audience tokenInfo /= ourClientId
          then Left "Client id didn't match"
          else Right (User (Email (email userInfo)))

validateToken :: Manager -> AccessToken -> IO (OAuth2Result GoogleToken)
validateToken manager token = parseResponseJSON <$> authGetBS' manager token uri
  where uri = "https://www.googleapis.com/oauth2/v2/tokeninfo"

getUserInfo :: Manager -> AccessToken -> IO (OAuth2Result GoogleUser)
getUserInfo manager token =
  authGetJSON manager token "https://www.googleapis.com/oauth2/v2/userinfo"
