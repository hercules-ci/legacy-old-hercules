{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

{-|
This module describes some static pages being used for testing.
-}
module Hercules.Static
  ( welcomePage
  , loginPage
  , loggedInPage
  , userInfoPage
  ) where


import Control.Monad.Except.Extra
import Control.Monad.Log
import Data.Foldable                 (toList)
import Data.Maybe
import Data.Semigroup
import Data.Text                     as T
import GitHub.Endpoints.Repos        as GH hiding (User)
import GitHub.Request
import Network.URI
import Servant
import Servant.Redirect
import Text.Blaze.Html               (Html)
import Text.InterpolatedString.Perl6
import Text.Markdown                 (defaultMarkdownSettings, markdown)

import Hercules.Database.Hercules
import Hercules.Encryption
import Hercules.Log
import Hercules.OAuth.Authenticators
import Hercules.OAuth.Types
import Hercules.OAuth.User
import Hercules.Query.Hercules
import Hercules.ServerEnv

welcomePage :: App Html
welcomePage = do
  hostAndPort <- getHostAndPort
  let stateString = "my state"
      frontendURL :: Text
      frontendURL = "http://" <> hostAndPort <> "/logged-in"
      uriGoogle, uriGitHub :: Text
      uriGoogle = [qc|/login/google?state={escapeURIString isUnescapedInURIComponent stateString}&frontendURL={frontendURL}|]
      uriGitHub = [qc|/login/github?state={escapeURIString isUnescapedInURIComponent stateString}&frontendURL={frontendURL}|]
  pure $ markdown defaultMarkdownSettings [qc|
# Login Page

## Parameters

- state: `{stateString}`
- frontendURL: `{frontendURL}`

## links:

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

userInfoPage :: UserId -> App Html
userInfoPage uid =
  runHerculesQueryWithConnectionSingular (userIdQuery uid) >>= \case
    Nothing -> pure $ noUserHtml uid
    Just u  -> reposHtml u

reposHtml :: User -> App Html
reposHtml User{..} = do
  logInfo (LogString ("Showing repos for " <> fromMaybe "unnamed user" userName))
  case userGithubToken of
    Nothing ->
      pure $ markdown defaultMarkdownSettings [qc|
# No github login for user
|]
    Just encryptedToken -> do
      token <- GH.OAuth <$> decrypt encryptedToken
      let repoRequest = currentUserReposR RepoPublicityAll FetchAll
      withHttpManager
        (\mgr -> executeRequestWithMgr mgr token repoRequest) >>= \case
        Left err ->
          pure $ markdown defaultMarkdownSettings [qc|
# Error getting repo list

{err}|]
        Right repos ->
          pure $ markdown defaultMarkdownSettings [qc|
# Repos for {fromMaybe "Unnamed user" userName}
{T.unlines . toList . fmap repoLine $ repos}|]

repoLine :: Repo -> Text
repoLine Repo{..} = [qc|- [{untagName repoName}]({getUrl repoHtmlUrl})|]

noUserHtml :: UserId -> Html
noUserHtml uid = markdown defaultMarkdownSettings [qc|
No user with id `{uid}`
|]
