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

authCallback :: AuthenticatorName
             -> Maybe AuthCode
             -> Maybe AuthError
             -> AuthStatePacked
             -> App a
authCallback authName maybeCode maybeError packedState = do
  -- Extract the state
  state <- failWith err400 (unpackState packedState)

  case (maybeCode, maybeError) of
    (Nothing, Nothing)   -> throwError err400
    (Nothing, Just err)  -> handleError state err
    (Just code, Nothing) -> handleCode authName state code
    (Just _, Just _)     -> throwError err400

handleError :: AuthState
            -> AuthError
            -> App a
handleError state err = do
  let redirectURI :: OA.URI
      redirectURI = encodeUtf8 . unFrontendURL . authStateFrontendURL $ state
  redirectError redirectURI (unAuthError err)

handleCode :: AuthenticatorName
           -> AuthState
           -> AuthCode
           -> App a
handleCode authName state (AuthCode code) = do
  -- Can we handle this authenticator
  authenticator <- failWithM err404 (getAuthenticator authName)
  let config = authenticatorConfig authenticator

  let clientState = authStateClientState state
      redirectURI :: OA.URI
      redirectURI = encodeUtf8 . unFrontendURL . authStateFrontendURL $ state
      failWithBS err = redirectError redirectURI (decodeUtf8 . toStrict $ err)

  -- Get the access token for this user
  token <- either failWithBS pure
    =<< withHttpManager (\m -> fetchAccessToken m config (encodeUtf8 code))

  -- Get the user info with the token
  user <- either (redirectError redirectURI) pure
    =<< authenticatorGetUserInfo authenticator token

  -- Create a JWT
  jwt <- either (const (redirectError redirectURI "Failed to create JWT")) pure
    =<< makeUserJWT user

  -- Return to the frontend
  redirectSuccess redirectURI jwt clientState

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
