{-# LANGUAGE Arrows          #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

{-|
A module to handle the different queries we might want to make to Hercules's
database
-}
module Hercules.Query.Hercules
  ( userIdQuery
  , insertUser
  ) where

import Control.Arrow              (returnA)
import Data.Text
import Database.PostgreSQL.Simple (Connection)
import Opaleye.Extra

import Hercules.Database.Hercules
import Hercules.OAuth.User

-- | A query to get a user by their github id
userIdQuery :: Text -> Query UserReadColumns
userIdQuery githubId = proc () -> do
  user@User{..} <- queryTable userTable -< ()
  restrict -< pgStrictText githubId `eqNullable` userGithubId
  returnA -< user

insertUser :: Connection -> User' a Text Text Text -> IO (Maybe UserId)
insertUser c User {userName
                  ,userEmail
                  ,userGithubId} =
  let user =
        User
          Nothing
          (Just (toNullable (pgStrictText userName)))
          (Just (toNullable (pgStrictText userEmail)))
          (Just (toNullable (pgStrictText userGithubId)))
  in runInsertManyReturning c userTable [user] userId >>=
     \case
       [i] -> pure $ Just (UserId i)
       _ -> pure Nothing
