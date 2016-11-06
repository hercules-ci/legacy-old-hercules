{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Hercules.OAuth
  ( AuthState(..)
  , AuthCode(..)
  , authCallback
  ) where

import           Control.Monad.Except.Extra
import           Data.Aeson
import           Data.ByteString.Lazy       (fromStrict, toStrict)
import           Data.Text
import           Data.Text.Encoding
import           Network.OAuth.OAuth2
import qualified Network.OAuth.OAuth2       as OA
import           Servant
import           Servant.Redirect

import Hercules.OAuth.Authenticators
import Hercules.OAuth.Types
import Hercules.ServerEnv

authCallback :: AuthenticatorName -> AuthStatePacked -> AuthCode -> App a
authCallback authName packedState (AuthCode code) = do
  -- Can we handle this authenticator
  authenticator <- failWithM err404 (getAuthenticator authName)
  let config = authenticatorConfig authenticator

  -- Extract the state
  state <- failWith err400 (unpackState packedState)
  let clientState = authStateClientState state
  let redirectURI :: OA.URI
      redirectURI = encodeUtf8 . unFrontendURL . authStateFrontendURL $ state
      failWithBS err = redirectError redirectURI (decodeUtf8 . toStrict $ err)

  -- Get the access token for this user
  withHttpManager (\m -> fetchAccessToken m config (encodeUtf8 code)) >>= \case
    Left err    -> failWithBS err
    Right token ->
      authenticatorGetUserInfo authenticator token >>= \case
        Left err -> redirectError redirectURI err
        Right user -> makeUserJWT user >>= \case
          Left _err  -> redirectError redirectURI "Failed to generate JWT"
          Right jwt -> redirectSuccess redirectURI jwt clientState

redirectError :: OA.URI
              -> Text
              -- ^ An error message
              -> App a
redirectError uri message =
  let param = [("authFailure", encodeUtf8 message)]
  in redirectBS (uri `appendQueryParam` param)

redirectSuccess :: OA.URI
                -> PackedJWT
                -- ^ This user's token
                -> Maybe AuthClientState
                -> App a
redirectSuccess uri jwt state =
  let params = ("jwt", unPackedJWT jwt) :
               case state of
                 Nothing -> []
                 Just s  -> [("state", encodeUtf8 . unAuthClientState $ s)]
  in redirectBS (uri `appendQueryParam` params)

unpackState :: AuthStatePacked -> Maybe AuthState
unpackState = decode . fromStrict . encodeUtf8 . unAuthStatePacked
