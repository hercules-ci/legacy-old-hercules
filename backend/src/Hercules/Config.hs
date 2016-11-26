{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE StrictData      #-}
{-# LANGUAGE TemplateHaskell #-}

module Hercules.Config
  ( Config(..)
  , ConnectInfo(..)
  , AuthClientInfo(..)
  , HostName
  , AccessLogLevel(..)
  ) where

import Data.Aeson
import Data.Aeson.TH
import Data.Char                  (toLower)
import Data.Text                  (Text)
import Database.PostgreSQL.Simple (ConnectInfo (..))
import GHC.Generics
import Network.Wai.Handler.Warp   (Port)

import Hercules.OAuth.Types

type HostName = Text

-- | Access logging level
data AccessLogLevel = Disabled | Enabled | Development
  deriving(Read, Show, Generic)

instance FromJSON AccessLogLevel

data Config = Config { configPort             :: Port
                     , configHostname         :: HostName
                     , configAccessLogLevel   :: AccessLogLevel
                     , configConnectionString :: Text
                     , configGoogleAuthInfo   :: Maybe AuthClientInfo
                     , configGitHubAuthInfo   :: Maybe AuthClientInfo
                     }
  deriving(Read, Show)

deriveFromJSON defaultOptions
  { fieldLabelModifier = \s ->
      case drop (length "config") s of
        []   -> []
        x:xs -> toLower x : xs
  }
  ''Config
