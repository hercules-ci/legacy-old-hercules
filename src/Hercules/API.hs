{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Hercules.API
  ( API
  , QueryAPI
  , Unprotected
  , Protected
  , User(..)
  ) where

import Data.Aeson
import Data.Text
import GHC.Generics        (Generic)
import Servant
import Servant.Auth.Server
import Servant.HTML.Blaze
import Text.Blaze.Html5

import Hercules.Database             (Project)
import Hercules.OAuth.Authenticators (AuthenticatorName)
import Hercules.OAuth.Types          (AuthClientState, AuthCode, AuthState)

data User = User { userName :: Text }
  deriving(Generic)

instance ToJSON User
instance ToJWT User
instance FromJSON User
instance FromJWT User

type Unprotected =
      "projectNames" :> Get '[JSON] [Text]
 :<|> "project" :> Capture "projectName" Text :> Get '[JSON] (Maybe Project)

type Protected = "protected" :> Get '[JSON] Text

type QueryAPI = Unprotected
      :<|> Auth '[JWT] User :> Protected

type Pages = "login" :> Get '[HTML] Html
        :<|> "login" :> Capture "authType" AuthenticatorName
                     :> QueryParam "state" AuthClientState
                     :> Get '[HTML] Html
        :<|> "auth-callback" :> Capture "authType" AuthenticatorName
                             :> QueryParam "state" AuthState
                             :> QueryParam "code" AuthCode
                             :> Get '[HTML] Html

type API = QueryAPI
      :<|> Pages
