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
  ) where

import Control.Monad.Except.Extra
import Control.Monad.Reader
import Data.List                       (find)
import Data.Pool
import Data.Profunctor.Product.Default (Default)
import Database.PostgreSQL.Simple      (Connection, close, connectPostgreSQL)
import Network.HTTP.Client             as HTTP
import Network.HTTP.Client.TLS
import Opaleye                         (Query, QueryRunner, runQuery)
import Servant                         (ServantErr)

import Hercules.Config
import Hercules.OAuth.Authenticators
import Hercules.OAuth.Types          (authenticatorName)

{-# ANN module "HLint: ignore Avoid lambda" #-}

data Env = Env { envConnectionPool :: Pool Connection
               , envHttpManager    :: HTTP.Manager
               , envAuthenticators :: [OAuth2Authenticator]
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

getAuthenticator :: AuthenticatorName -> App (Maybe OAuth2Authenticator)
getAuthenticator name =
  find ((== name) . authenticatorName) <$> asks envAuthenticators

-- | Evaluate a query in an 'App' value
runQueryWithConnection
  :: Default QueryRunner columns haskells => Query columns -> App [haskells]
runQueryWithConnection q = withConnection (\c -> runQuery c q)

runApp :: Env -> App a -> ExceptT ServantErr IO a
runApp env = flip runReaderT env . unApp

newEnv :: MonadIO m => Config -> m Env
newEnv c@Config{..} = liftIO $ do
  connection <- createPool
    (connectPostgreSQL configConnectionString)
    close
    4 10 4
  httpManager <- newManager tlsManagerSettings
  pure $ Env
    connection
    httpManager
    (configAuthenticatorList c)
