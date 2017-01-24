{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

module Hercules.Database.Hercules.Migration
  ( readyDatabase
  , MigrationResult(..)
  , Verbosity(..)
  ) where

import Data.FileEmbed
import Data.Text
import Database.PostgreSQL.Simple           (Connection, withTransaction)
import Database.PostgreSQL.Simple.Migration

-- | Prepare the database for Hercules use
readyDatabase :: Verbosity -> Connection -> IO (MigrationResult Text)
readyDatabase verbosity con =
  fmap (fmap pack)
  . withTransaction con
  . runMigrations (verbosityToBool verbosity) con
  $ migrations

data Verbosity = Verbose | Quiet

verbosityToBool :: Verbosity -> Bool
verbosityToBool = \case
  Verbose -> True
  Quiet -> False

-- | The migrations to get an empty database in a usable state
--
-- It's really important that one doesn't change the elements of this list and
-- only appends to it as the hashes of these commands are stored and used to
-- verify that the database is in a good state.
migrations :: [MigrationCommand]
migrations =
  [ MigrationInitialization
  , MigrationScript "Create the users table"
                    $(embedFile "src/migrations/create-users.sql")
  ]
