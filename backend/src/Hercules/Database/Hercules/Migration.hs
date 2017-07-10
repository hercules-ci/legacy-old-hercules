{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Hercules.Database.Hercules.Migration
  ( readyDatabase
  , MigrationResult(..)
  , Verbosity(..)
  ) where

import Control.Monad                        (void, when)
import Data.FileEmbed
import Data.Text
import Database.PostgreSQL.Simple           (Connection, execute,
                                             withTransaction)
import Database.PostgreSQL.Simple.Migration

-- | Prepare the database for Hercules use
readyDatabase :: Verbosity -> Connection -> IO (MigrationResult Text)
readyDatabase verbosity con =
  fmap (fmap pack) . withTransaction con $ do
    -- Silence any informational warnings.
    when (verbosity == Quiet) $
      void (execute con "SET LOCAL client_min_messages TO warning;" ())

    -- Ready the database
    runMigrations (verbosityToBool verbosity) con migrations

data Verbosity = Verbose | Quiet
  deriving (Eq)

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
  , MigrationScript "Recreate the users table"
                    $(embedFile "src/migrations/20170709_recreate-users.sql")
  ]
