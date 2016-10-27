{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData                 #-}

module Hercules.ServerEnv
  ( Env(..)
  , App(..)
  , runApp
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import Database.PostgreSQL.Simple (Connection)
import Network.OAuth.OAuth2       (OAuth2)
import Servant                    (ServantErr)

data Env = Env { envConnection  :: Connection
               , envGoogleOAuth :: OAuth2
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

runApp :: Env -> App a -> ExceptT ServantErr IO a
runApp env = flip runReaderT env . unApp

