{-# LANGUAGE StrictData #-}

module Hercules.Config
  ( Config(..)
  , ConnectInfo(..)
  , AuthClientInfo(..)
  , HostName
  ) where

import Data.ByteString
import Data.Text
import Database.PostgreSQL.Simple (ConnectInfo (..))
import Network.Wai.Handler.Warp   (Port)

import Hercules.OAuth.Types

type HostName = Text

data Config = Config { configPort             :: Port
                     , configHostName         :: HostName
                     , configConnectionString :: ByteString
                     , configGoogleAuthInfo   :: Maybe AuthClientInfo
                     , configGitHubAuthInfo   :: Maybe AuthClientInfo
                     }
  deriving(Read, Show)

