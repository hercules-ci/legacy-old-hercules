{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData    #-}

{-|
This module describes a data type and functions for dealing with authenticated
users.
-}
module Hercules.OAuth.User
  ( UserId(..)
  ) where

import Data.Aeson
import Data.Int
import GHC.Generics        (Generic)
import Servant.Auth.Server

newtype UserId = UserId
  { unUserId :: Int64 }
  deriving(Show, Generic)

instance ToJSON UserId
instance ToJWT UserId
instance FromJSON UserId
instance FromJWT UserId
