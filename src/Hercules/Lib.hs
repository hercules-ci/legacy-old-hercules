{-# LANGUAGE Arrows            #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module Hercules.Lib
  ( startApp
  ) where

import           Control.Arrow               (returnA)
import           Control.Monad               (join)
import           Control.Monad.IO.Class
import           Control.Monad.Reader        (asks)
import           Data.ByteString
import           Data.ByteString.Lazy        (toStrict)
import           Data.Monoid                 ((<>))
import           Data.Text
import           Data.Text.Encoding
import           Database.PostgreSQL.Simple  (connectPostgreSQL)
import           Network.HTTP.Client         (Manager, newManager)
import           Network.HTTP.Client.TLS     (tlsManagerSettings)
import           Network.OAuth.OAuth2
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Opaleye
import           Safe                        (headMay)
import           Servant
import           Servant.Auth.Server
import           Text.Blaze.Html5            (Html, body, docTypeHtml, p, ul,
                                              unsafeByteStringValue, (!))
import qualified Text.Blaze.Html5            as H
import           Text.Blaze.Html5.Attributes (href)

import Hercules.API
import Hercules.Config
import Hercules.Database     (Project, Project' (..), ProjectReadColumns,
                              projectTable)
import Hercules.OAuth
import Hercules.OAuth.Google
import Hercules.ServerEnv

startApp :: Config -> IO ()
startApp Config{..} = do
  connection <- connectPostgreSQL configConnectionString
  let env = Env connection
        (googleOAuthSettings configHostName configPort configGoogleAuthInfo)
  run configPort =<< app env

app :: Env -> IO Application
app env = do
  key <- generateKey
  let api = Proxy :: Proxy API
      jwtConfig = defaultJWTSettings key
      authConfig = defaultCookieSettings :. jwtConfig :. EmptyContext
  print =<< makeJWT (User "joe") jwtConfig Nothing
  pure $ serveWithContext api authConfig (server env)

server :: Env -> Server API
server env = enter (Nat (runApp env)) api
  where api = queryApi :<|> pages
        pages = loginPage :<|> authPage
        queryApi = unprotected :<|> protected
        unprotected = getProjectNames
                 :<|> getProject
        protected = getUser

getProjectNames :: App [Text]
getProjectNames = do
  connection <- asks envConnection
  liftIO $ runQuery connection nameQuery

  where
    nameQuery :: Query (Column PGText)
    nameQuery = proc () -> do
      Project{..} <- queryTable projectTable -< ()
      returnA -< projectName

getProject :: Text -> App (Maybe Project)
getProject name = do
  connection <- asks envConnection
  projects <- liftIO $ runQuery connection projectQuery
  pure $ headMay projects

  where

    projectQuery :: Query ProjectReadColumns
    projectQuery = proc () -> do
      project@Project{..} <- queryTable projectTable -< ()
      restrict -< projectName .== pgStrictText name
      returnA -< project

getUser :: AuthResult User -> App Text
getUser = \case
  (Authenticated (User name)) -> pure name
  _                           -> throwAll err401

loginPage :: App Html
loginPage = do
  let state = AuthState "my state"
  authURL <- googleAuthURL state <$> asks envGoogleOAuth
  pure $ docTypeHtml $ do
    H.head $
      H.title "Login with Google"
    body $
      (H.a ! href (unsafeByteStringValue authURL)) "login"

authPage :: Maybe AuthState -> Maybe AuthCode -> App Html
authPage a b = join (authPage' <$> mandatory a <*> mandatory b)

  where

    authPage' :: AuthState -> AuthCode -> App Html
    authPage' (AuthState state) (AuthCode code) = do
      googleOAuth <- asks envGoogleOAuth
      manager <- liftIO $ newManager tlsManagerSettings
      liftIO (fetchAccessToken manager googleOAuth (encodeUtf8 code)) >>= \case
        Left bs -> authFailure (toStrict bs)
        Right token -> authSuccess manager token

      where

        authFailure :: ByteString -> App Html
        authFailure reason =
          pure $ docTypeHtml $ do
            H.head $
              H.title "Auth failure!"
            body $
               p ("Auth failure: " <> H.toHtml (decodeUtf8 reason))

        authSuccess :: Manager -> AccessToken -> App Html
        authSuccess manager token =
          validateToken manager token >>= \case
          Left bs -> authFailure (toStrict bs)
          Right validated ->
            userinfo manager token >>= \case
              Left bs -> authFailure (toStrict bs)
              Right info ->
                pure $ docTypeHtml $ do
                  H.head $
                    H.title "Redirected!"
                  body $
                    ul $ do
                     p ("state : " <> H.toHtml state)
                     p ("code : " <> H.toHtml code)
                     p ("Access token : " <> H.toHtml (show token))
                     p ("validated : " <> H.toHtml (show validated))
                     p ("info : " <> H.toHtml (show info))

-- | Token Validation
validateToken :: Manager
              -> AccessToken
              -> App (OAuth2Result ByteString)
validateToken manager token = liftIO . fmap (fmap toStrict) $
  authGetBS' manager token url
  where url = "https://www.googleapis.com/oauth2/v1/tokeninfo"

userinfo :: Manager
         -> AccessToken
         -> App (OAuth2Result ByteString)
userinfo manager token = liftIO . fmap (fmap toStrict) $
  authGetBS manager token url
  where url = "https://www.googleapis.com/oauth2/v2/userinfo"

mandatory :: Maybe a -> App a
mandatory = \case
  Nothing -> throwAll err400
  Just x -> pure x
