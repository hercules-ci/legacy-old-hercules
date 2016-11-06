{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

{-|
This module describes some static pages being used for testing.
-}
module Hercules.Static
  ( welcomePage
  , loginPage
  ) where


import Control.Monad.Except.Extra
import Data.Aeson
import Data.ByteString.Lazy          (toStrict)
import Data.Text
import Data.Text.Encoding
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
welcomePage = do
  let stateString = "my state"
  let uri :: Text
      uri = [qc|/login/google?state={escapeURIString isUnescapedInURIComponent stateString}|]
  pure $ markdown defaultMarkdownSettings [qc|
# Login Page

Logging in with state: {stateString}

[{uri}]({uri})
|]

-- | This is a redirect for authenticating with google with the given user
-- state.
loginPage :: AuthenticatorName -> Maybe AuthClientState -> App a
loginPage name stateString = do
  state <- makeState stateString
  authenticator <- failWith err404 (getAuthenticator name)
  let authURL = authenticationURLWithState authenticator state
  redirectBS (unUserAuthURL authURL)

makeState :: Maybe AuthClientState -> App AuthState
makeState = pure
            . AuthState
            . decodeUtf8
            . toStrict
            . encode
            . fmap unAuthClientState
