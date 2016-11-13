{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Hercules.API
  ( API
  , QueryAPI
  , Unprotected
  , Protected
  , User(..)
  ) where

import Data.Text
import Servant
import Servant.Auth.Server
import Servant.HTML.Blaze
import Text.Blaze.Html5

import Hercules.Database             (Project)
import Hercules.OAuth.Authenticators (AuthenticatorName)
import Hercules.OAuth.Types          (AuthClientState, AuthCode, AuthError,
                                      AuthStatePacked, FrontendURL)
import Hercules.OAuth.User

type Unprotected =
      "projectNames" :> Get '[JSON] [Text]
 :<|> "project" :> Get '[JSON] [Project]
 :<|> "project" :> Capture "projectName" Text :> Get '[JSON] (Maybe Project)

type Protected = "protected" :> Get '[JSON] Text

type QueryAPI = Unprotected
      :<|> Auth '[JWT] User :> Protected

type Pages = "login" :> Get '[HTML] Html
        :<|> "login" :> Capture "authType" AuthenticatorName
                     :> QueryParam "state" AuthClientState
                     :> QueryParam "frontendURL" FrontendURL
                     :> Get '[HTML] Html
        :<|> "auth-callback" :> Capture "authType" AuthenticatorName
                             :> QueryParam "code" AuthCode
                             :> QueryParam "error" AuthError
                             :> QueryParam "state" AuthStatePacked
                             :> Get '[HTML] Html
        :<|> "logged-in" :> QueryParam "jwt" Text
                         :> Get '[HTML] Html


type API = QueryAPI
      :<|> Pages
