{-# LANGUAGE StrictData #-}

module Hercules.Config
  ( Config(..)
  ) where

import Network.Wai.Handler.Warp (Port)

data Config = Config { configPort :: Port
                     }
  deriving(Read, Show)
