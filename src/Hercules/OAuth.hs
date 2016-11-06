{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Hercules.OAuth
  ( AuthState(..)
  , AuthCode(..)
  , authPage
  ) where

import Control.Monad.Except.Extra
import Data.ByteString
import Data.ByteString.Lazy          (toStrict)
import Data.Text.Encoding
import Network.OAuth.OAuth2
import Servant
import Text.Blaze.Html               (Html)
import Text.InterpolatedString.Perl6
import Text.Markdown                 (defaultMarkdownSettings, markdown)

import Hercules.OAuth.Authenticators
import Hercules.OAuth.Types
import Hercules.ServerEnv

authPage :: AuthenticatorName -> AuthState -> AuthCode -> App Html
authPage authName (AuthState state) (AuthCode code) = do
  -- See if we can handle this particular authenticator
  authenticator <- failWith err404 (getAuthenticator authName)
  let config = authenticatorConfig authenticator

  withHttpManager (\m -> fetchAccessToken m config (encodeUtf8 code)) >>= \case
    Left bs -> authFailure (toStrict bs)
    Right token -> authSuccess token

  where

    authFailure :: ByteString -> App Html
    authFailure reason = pure $ markdown defaultMarkdownSettings [qc|
# Authentication Failure

## Reason

{decodeUtf8 reason}
|]

    authSuccess :: AccessToken -> App Html
    authSuccess token =
      validateToken token >>= \case
      Left bs -> authFailure (toStrict bs)
      Right validated ->
        userinfo token >>= \case
          Left bs -> authFailure (toStrict bs)
          Right info -> pure $ markdown defaultMarkdownSettings [qc|
# Redirected

- state: `{state}`

- code: `{code}`

- token: `{token}`

- validated: `{validated}`

- info: `{info}`
|]

-- | Token Validation
validateToken :: AccessToken
              -> App (OAuth2Result ByteString)
validateToken token = fmap toStrict <$>
  withHttpManager (\m -> authGetBS' m token url)
  where url = "https://www.googleapis.com/oauth2/v1/tokeninfo"

userinfo :: AccessToken
         -> App (OAuth2Result ByteString)
userinfo token = fmap toStrict <$>
  withHttpManager (\m -> authGetBS m token url)
  where url = "https://www.googleapis.com/oauth2/v2/userinfo"

