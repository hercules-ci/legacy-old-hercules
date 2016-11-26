{-# LANGUAGE StrictData      #-}
{-# LANGUAGE TemplateHaskell #-}

module Hercules.Config
  ( Config(..)
  , ConnectInfo(..)
  , AuthClientInfo(..)
  , HostName
  ) where

import Data.Aeson.TH
import Data.Char                  (toLower)
import Data.Text                  (Text)
import Database.PostgreSQL.Simple (ConnectInfo (..))
import Network.Wai.Handler.Warp   (Port)

import Hercules.OAuth.Types

type HostName = Text

data Config = Config { configPort             :: Port
                     , configHostname         :: HostName
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
