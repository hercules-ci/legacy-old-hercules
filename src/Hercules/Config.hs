{-# LANGUAGE StrictData #-}

module Hercules.Config
  ( Config(..)
  , ConnectInfo(..)
  ) where

import Data.ByteString
import Database.PostgreSQL.Simple (ConnectInfo (..))
import Network.Wai.Handler.Warp   (Port)

data Config = Config { configPort             :: Port
                     , configConnectionString :: ByteString
                     }
  deriving(Read, Show)
