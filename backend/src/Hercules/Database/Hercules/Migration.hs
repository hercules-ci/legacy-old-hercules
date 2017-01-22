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

-- | Run a series of migrations
runMigrations
  :: Bool
     -- ^ Run in verbose mode
  -> Connection
     -- ^ The postgres connection to use
  -> [MigrationCommand]
     -- ^ The commands to run
  -> IO (MigrationResult String)
runMigrations verbose con commands =
  sequenceContexts [MigrationContext c verbose con | c <- commands]

-- | Run a sequence of contexts, stopping on the first failure
sequenceContexts :: [MigrationContext] -> IO (MigrationResult String)
sequenceContexts = \case
  []   -> pure MigrationSuccess
  c:cs -> do
    r <- runMigration c
    case r of
      MigrationError s -> pure (MigrationError s)
      MigrationSuccess -> sequenceContexts cs

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
