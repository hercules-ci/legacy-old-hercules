{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StrictData                 #-}

module Hercules.ServerEnv
  ( Env(..)
  , App(..)
  , runApp
  , newEnv
  , runHerculesQueryWithConnection
  , runHerculesQueryWithConnectionSingular
  , runHydraQueryWithConnection
  , withHerculesConnection
  , withHttpManager
  , getAuthenticator
  , makeUserJWT
  ) where

import Control.Monad.Except.Extra
import Control.Monad.Log
import Control.Monad.Reader
import Crypto.JOSE.Error
import Data.ByteString.Lazy            (toStrict)
import Data.List                       (find)
import Data.Maybe                      (fromMaybe)
import Data.Pool
import Data.Profunctor.Product.Default (Default)
import Data.String                     (fromString)
import Data.Text.Encoding              (encodeUtf8)
import Data.Time.Format
import Database.PostgreSQL.Simple      (Connection, close, connectPostgreSQL)
import Network.HTTP.Client             as HTTP
import Network.HTTP.Client.TLS
import Opaleye                         (Query, QueryRunner, Unpackspec,
                                        runQuery, showSql)
import Servant                         (ServantErr)
import Servant.Auth.Server             (JWTSettings, defaultJWTSettings,
                                        generateKey, makeJWT)

import Hercules.Database.Hercules.Ready
import Hercules.Config
import Hercules.Log
import Hercules.OAuth.Types (AuthenticatorName, OAuth2Authenticator,
                             PackedJWT (..), authenticatorName)
import Hercules.OAuth.User

{-# ANN module ("HLint: ignore Avoid lambda" :: String) #-}

data Env = Env { envHerculesConnectionPool :: Pool Connection
               , envHydraConnectionPool :: Pool Connection
               , envHttpManager    :: HTTP.Manager
               , envAuthenticators :: [OAuth2Authenticator App]
               , envJWTSettings    :: JWTSettings
               }

newtype App a = App
  { unApp :: ReaderT Env (ExceptT ServantErr (LogM (WithSeverity LogMessage) IO)) a
  }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadError ServantErr
           , MonadIO
           , MonadLog (WithSeverity LogMessage)
           , MonadReader Env
           )

-- | Perform an action with a PostgreSQL connection to the Hercules DB and
-- return the result
withHerculesConnection :: (Connection -> IO a) -> App a
withHerculesConnection f = do
  connectionPool <- asks envHerculesConnectionPool
  liftIO $ withResource connectionPool f

-- | Perform an action with a PostgreSQL connection to the Hydra DB and return
-- the result
withHydraConnection :: (Connection -> IO a) -> App a
withHydraConnection f = do
  connectionPool <- asks envHydraConnectionPool
  liftIO $ withResource connectionPool f

withHttpManager :: (HTTP.Manager -> IO a) -> App a
withHttpManager f = do
  manager <- asks envHttpManager
  liftIO $ f manager

getAuthenticator :: AuthenticatorName -> App (Maybe (OAuth2Authenticator App))
getAuthenticator name =
  find ((== name) . authenticatorName) <$> asks envAuthenticators

makeUserJWT :: UserId -> App (Either Error PackedJWT)
makeUserJWT user = do
  jwtSettings <- asks envJWTSettings
  liftIO $ fmap (PackedJWT . toStrict) <$> makeJWT user jwtSettings Nothing

-- | Evaluate a query in an 'App' value
runHerculesQueryWithConnection
  :: Default QueryRunner columns haskells
  => Default Unpackspec columns columns
  => Query columns -> App [haskells]
runHerculesQueryWithConnection q = do
  logQuery q
  withHerculesConnection (\c -> runQuery c q)

-- | Evaluate a query in an 'App' value returning a singular result
runHerculesQueryWithConnectionSingular
  :: Default QueryRunner columns haskells
  => Default Unpackspec columns columns =>
     Query columns -> App (Maybe haskells)
runHerculesQueryWithConnectionSingular q =
  runHerculesQueryWithConnection q >>=
  \case
    []  -> pure Nothing
    [x] -> pure $ Just x
    _   -> do
      logError "Singular query returned multiple results"
      pure Nothing

-- | Evaluate a query in an 'App' value
runHydraQueryWithConnection
  :: Default QueryRunner columns haskells
  => Default Unpackspec columns columns
  => Query columns -> App [haskells]
runHydraQueryWithConnection q = do
  logQuery q
  withHydraConnection (\c -> runQuery c q)

logQuery
  :: Default Unpackspec columns columns
  => Query columns
  -> App ()
logQuery q =
  let s = fromMaybe "Empty query" $ showSql q
  in logDebug (fromString s)

runApp :: Env -> App a -> ExceptT ServantErr IO a
runApp env = mapExceptT runLog
           . flip runReaderT env
           . unApp
  where
    runLog :: LogM (WithSeverity LogMessage) IO a -> IO a
    runLog = (`runLoggingT` printMessage) . mapLogMessageM timestamp
    printMessage :: WithTimestamp (WithSeverity LogMessage) -> IO ()
    printMessage = print . renderWithTimestamp renderTime (renderWithSeverity render)
    renderTime = formatTime defaultTimeLocale "%b %_d %H:%M:%S"

getHerculesConnection :: MonadIO m => Config -> m (Maybe (Pool Connection))
getHerculesConnection Config{..} = liftIO $ do
  herculesConnection <- createPool
    (connectPostgreSQL (encodeUtf8 configHerculesConnectionString))
    close
    4 10 4
  withResource herculesConnection (readyDatabase Quiet) >>= \case
    MigrationError s -> do
      putStrLn ("Error migrating hercules database: " ++ s)
      pure Nothing
    MigrationSuccess -> pure (Just herculesConnection)

newEnv :: MonadIO m => Config -> [OAuth2Authenticator App] -> m (Maybe Env)
newEnv c@Config{..} authenticators =
  getHerculesConnection c >>= \case
    Nothing -> pure Nothing
    Just herculesConnection -> fmap Just . liftIO $ do
      hydraConnection <- createPool
        (connectPostgreSQL (encodeUtf8 configHydraConnectionString))
        close
        4 10 4
      httpManager <- newManager tlsManagerSettings
      key <- liftIO generateKey
      let jwtSettings = defaultJWTSettings key
      pure $ Env
        herculesConnection
        hydraConnection
        httpManager
        authenticators
        jwtSettings
