{-# LANGUAGE StrictData #-}

module Hercules.Config
  ( Config(..)
  , ConnectInfo(..)
  , AuthClientInfo(..)
  , HostName
  , AccessLogLevel(..)
  ) where

import Data.ByteString
import Data.Text
import Database.PostgreSQL.Simple (ConnectInfo (..))
import Network.Wai.Handler.Warp   (Port)

import Hercules.OAuth.Types

type HostName = Text

data Config = Config { configPort             :: Port
                     , configHostName         :: HostName
                     , configAccessLogLevel   :: AccessLogLevel
                     , configConnectionString :: ByteString
                     , configGoogleAuthInfo   :: Maybe AuthClientInfo
                     , configGitHubAuthInfo   :: Maybe AuthClientInfo
                     }
  deriving(Read, Show)

-- | Access logging level
data AccessLogLevel = Disabled | Enabled | Development
  deriving(Read, Show)
