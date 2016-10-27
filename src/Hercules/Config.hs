{-# LANGUAGE StrictData #-}

module Hercules.Config
  ( Config(..)
  , ConnectInfo(..)
  , AuthInfo(..)
  , HostName
  ) where

import Data.ByteString
import Data.Text
import Database.PostgreSQL.Simple (ConnectInfo (..))
import Network.Wai.Handler.Warp   (Port)

type HostName = Text

data Config = Config { configPort             :: Port
                     , configHostName         :: HostName
                     , configConnectionString :: ByteString
                     , configGoogleAuthInfo   :: AuthInfo
                     }
  deriving(Read, Show)

data AuthInfo = AuthInfo { authInfoClientId     :: ByteString
                         , authInfoClientSecret :: ByteString
                         }
  deriving(Read, Show)
