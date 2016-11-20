{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

{-|
This module describes some static pages being used for testing.
-}
module Hercules.Static
  ( welcomePage
  , loginPage
  , loggedInPage
  ) where


import Control.Monad.Except.Extra
import Data.Text
import Network.URI
import Servant
import Servant.Redirect
import Text.Blaze.Html               (Html)
import Text.InterpolatedString.Perl6
import Text.Markdown                 (defaultMarkdownSettings, markdown)

import Hercules.OAuth.Authenticators
import Hercules.OAuth.Types
import Hercules.ServerEnv

welcomePage :: App Html
welcomePage =
  let stateString = "my state"
      frontendURL :: Text
      frontendURL = "http://localhost:8080/logged-in"
      uriGoogle, uriGitHub :: Text
      uriGoogle = [qc|/login/google?state={escapeURIString isUnescapedInURIComponent stateString}&frontendURL={frontendURL}|]
      uriGitHub = [qc|/login/github?state={escapeURIString isUnescapedInURIComponent stateString}&frontendURL={frontendURL}|]
  in pure $ markdown defaultMarkdownSettings [qc|
# Login Page

Logging in with

- state: `{stateString}`
- frontendURL: `{frontendURL}`

- [google]({uriGoogle})
- [github]({uriGitHub})
|]

loggedInPage :: Maybe Text -> App Html
loggedInPage jwt =
  pure $ markdown defaultMarkdownSettings [qc|
# Logged in!

JWT: `{jwt}`
|]

-- | This is a redirect for authenticating with google with the given user
-- state.
loginPage :: AuthenticatorName -> Maybe AuthClientState -> FrontendURL -> App a
loginPage name stateString frontendURL = do
  state <- makeState frontendURL stateString
  authenticator <- failWithM err404 (getAuthenticator name)
  let authURL = authenticationURLWithState authenticator state
  redirectBS (unUserAuthURL authURL)

makeState :: FrontendURL -> Maybe AuthClientState -> App AuthState
makeState frontendURL = pure . AuthState frontendURL
