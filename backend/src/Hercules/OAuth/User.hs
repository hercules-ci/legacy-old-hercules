{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData    #-}

{-|
This module describes a data type and functions for dealing with authenticated
users.
-}
module Hercules.OAuth.User
  ( User(..)
  , Email(..)
  ) where

import Data.Aeson
import Data.Text
import GHC.Generics        (Generic)
import Servant.Auth.Server

data User = User
  { userEmail :: Email }
  deriving(Generic)

instance ToJSON User
instance ToJWT User
instance FromJSON User
instance FromJWT User

newtype Email = Email
  { unEmail :: Text }
  deriving(Generic)

instance ToJSON Email
instance ToJWT Email
instance FromJSON Email
instance FromJWT Email
