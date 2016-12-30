{-# LANGUAGE Arrows          #-}
{-# LANGUAGE RecordWildCards #-}

{-|
A module to handle the different queries we might want to make to Hercules's
database
-}
module Hercules.Query.Hercules
  ( userIdQuery
  ) where

import Control.Arrow (returnA)
import Data.Text
import Opaleye

import Hercules.Database.Hercules

-- | A query to get a user by their github id
userIdQuery :: Text -> Query UserReadColumns
userIdQuery githubId = proc () -> do
  user@User{..} <- queryTable userTable -< ()
  restrict -< pgStrictText githubId `eqNullable` userGithubId
  returnA -< user

eqNullable :: Column a -> Column (Nullable a) -> Column PGBool
eqNullable a = matchNullable (pgBool False) (.== a)
