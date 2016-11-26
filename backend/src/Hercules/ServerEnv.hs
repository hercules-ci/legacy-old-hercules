{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StrictData                 #-}

module Hercules.ServerEnv
  ( Env(..)
  , App(..)
  , runApp
  , newEnv
  , runQueryWithConnection
  , withHttpManager
  , getAuthenticator
  , makeUserJWT
  ) where

import Control.Monad.Except.Extra
import Control.Monad.Reader
import Crypto.JOSE.Error
import Data.ByteString.Lazy            (toStrict)
import Data.List                       (find)
import Data.Pool
import Data.Profunctor.Product.Default (Default)
import Data.Text.Encoding              (encodeUtf8)
import Database.PostgreSQL.Simple      (Connection, close, connectPostgreSQL)
import Network.HTTP.Client             as HTTP
import Network.HTTP.Client.TLS
import Opaleye                         (Query, QueryRunner, runQuery)
import Servant                         (ServantErr)
import Servant.Auth.Server             (JWTSettings, defaultJWTSettings,
                                        generateKey, makeJWT)

import Hercules.Config
-- import Hercules.OAuth.Authenticators
import Hercules.OAuth.Types (AuthenticatorName, OAuth2Authenticator,
                             PackedJWT (..), authenticatorName)
import Hercules.OAuth.User

{-# ANN module "HLint: ignore Avoid lambda" #-}

data Env = Env { envConnectionPool :: Pool Connection
               , envHttpManager    :: HTTP.Manager
               , envAuthenticators :: [OAuth2Authenticator App]
               , envJWTSettings    :: JWTSettings
               }

newtype App a = App
  { unApp :: ReaderT Env (ExceptT ServantErr IO) a
  }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader Env
           , MonadError ServantErr
           , MonadIO
           )

-- | Perform an action with a PostgreSQL connection and return the result
withConnection :: (Connection -> IO a) -> App a
withConnection f = do
  connectionPool <- asks envConnectionPool
  liftIO $ withResource connectionPool f

withHttpManager :: (HTTP.Manager -> IO a) -> App a
withHttpManager f = do
  manager <- asks envHttpManager
  liftIO $ f manager

getAuthenticator :: AuthenticatorName -> App (Maybe (OAuth2Authenticator App))
getAuthenticator name =
  find ((== name) . authenticatorName) <$> asks envAuthenticators

makeUserJWT :: User -> App (Either Error PackedJWT)
makeUserJWT user = do
  jwtSettings <- asks envJWTSettings
  liftIO $ fmap (PackedJWT . toStrict) <$> makeJWT user jwtSettings Nothing

-- | Evaluate a query in an 'App' value
runQueryWithConnection
  :: Default QueryRunner columns haskells => Query columns -> App [haskells]
runQueryWithConnection q = withConnection (\c -> runQuery c q)

runApp :: Env -> App a -> ExceptT ServantErr IO a
runApp env = flip runReaderT env . unApp

newEnv :: MonadIO m => Config -> [OAuth2Authenticator App] -> m Env
newEnv Config{..} authenticators = liftIO $ do
  connection <- createPool
    (connectPostgreSQL (encodeUtf8 configConnectionString))
    close
    4 10 4
  httpManager <- newManager tlsManagerSettings
  key <- liftIO generateKey
  let jwtSettings = defaultJWTSettings key
  pure $ Env
    connection
    httpManager
    authenticators
    jwtSettings
