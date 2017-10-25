{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Nix.Build
  (
  -- * Types
    Derivation(..)
  , NixPath(..)
  , NixException(..)
  -- * Evaluation
  , evaluate
  -- * Realization
  , realize
  -- * Debugging tools
  , build
  , run
  ) where

import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Log
import           Data.ByteString              as BS
import           Data.ByteString.Char8        as BS8
import           Data.ByteString.Lazy.Char8   as BSL8
import           Data.Semigroup
import           Data.Text                    as T
import           Data.Typeable
import qualified Data.Vector                  as V
import           Nix.Build.Paths
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.IO.Temp
import           System.Process.Typed
import           Text.PrettyPrint.Leijen.Text (textStrict)

-- | A file containing a derivation.
--
-- This may be the path to a symbolic link to a store path.
newtype Derivation = Derivation { unDerivation :: FilePath }
  deriving Show

-- | Something to go in the NIX_PATH variable
newtype NixPath = NixPath { unNixPath :: BS.ByteString }
  deriving Show

newtype NixException = NixException { unNixException :: Text }
  deriving (Show, Typeable)

instance Exception NixException

-- | A helper for running the actions in this module
run
  :: Show a
  => (LoggingT (WithSeverity Text) IO) a -> IO a
run a = runLoggingT a (print . renderWithSeverity textStrict)

-- | Use @evaluate@ and @realize@ to build a nix expression
build
  :: ( MonadThrow m
     , MonadIO m
     , MonadLog (WithSeverity Text) m
     , MonadMask m
     )
  => FilePath
  -- ^ The path to a file containing a Nix Expresion to evaluate.
  -> Maybe FilePath
  -- ^ The path to a directory in which to symlink roots to the generated
  -- paths. A unique directory will be created in this path and the
  -- paths links placed in there.
  -> Maybe NixPath
  -- ^ What to set the NIX_PATH environment variable to
  -> m (V.Vector FilePath)
  -- ^ The realised store paths
build expression rootDir nixPathVar =
  -- Construct a temporary directory for the evaluated derivations
  withSystemTempDirectory "drvs" $ \drvRoot -> do
    drvs <- evaluate expression (Just drvRoot) nixPathVar
    join <$> traverse (`realize` rootDir) drvs

-- | Use @nix-instantiate@ to evaluate a nix expression, optionally adding
-- roots for the generated derivations.
evaluate
  :: ( MonadThrow m
     , MonadIO m
     , MonadLog (WithSeverity Text) m
     )
  => FilePath
  -- ^ The path to a file containing a Nix Expresion to evaluate.
  -> Maybe FilePath
  -- ^ The path to a directory in which to symlink roots to the generated
  -- derivations. A unique directory will be created in this path and the
  -- derivation links placed in there.
  -> Maybe NixPath
  -- ^ What to set the NIX_PATH environment variable to
  -> m (V.Vector Derivation)
  -- ^ The derivations represented in this expression
evaluate expression rootDir nixPathVar = do
  logInfo ("Evaluating " <> T.pack expression)

  -- Create a directory to put the roots in and get the appropriate flags to
  -- pass to nix-instantiate.
  rootFlags <- case rootDir of
    Nothing      -> do
      logWarning "Not adding roots during evaluation"
      pure []
    Just rootDir -> getRootFlags rootDir

  -- Call nix-instantiate to evaluate the expression
  let args = expression : rootFlags
      env = [ ("LD_PRELOAD", herculesStoreLibPath)
            , ("NIX_REMOTE", "hercules://")
            ] ++
            [ ("NIX_PATH", BS8.unpack nixPathVar)
            | Just (NixPath nixPathVar) <- pure nixPathVar
            ]
      inst = setStdout byteStringOutput
           . setStdin closed
           . setStderr inherit
           . setEnv env
           $ proc (nixPath </> "bin" </> "nix-instantiate") args

  logDebug ("Calling nix-instantiate with " <> T.pack (show args))
  (code, stdout) <- readProcessStdOut inst

  -- Abort on failure
  throwOnFailure "nix-instantiate" code

  -- Return the 'Derivation's in a 'Vector'
  pure . fmap (Derivation . BSL8.unpack) . V.fromList . BSL8.lines $ stdout

-- | Use @nix-store --realize@ to build the outputs specified by a derivation
realize
  :: ( MonadThrow m
     , MonadIO m
     , MonadLog (WithSeverity Text) m
     )
  => Derivation
  -- ^ The path to the derivation to realise
  -> Maybe FilePath
  -- ^ The path to a directory in which to symlink roots to the generated
  -- paths. A unique directory will be created in this path and the path links
  -- placed in there.
  -> m (V.Vector FilePath)
  -- ^ The realised store paths
realize derivation rootDir = do
  logInfo ("Realizing " <> T.pack (unDerivation derivation))

  -- Create a directory to put the roots in and get the appropriate flags to
  -- pass to nix-store.
  rootFlags <- case rootDir of
    Nothing      -> do
      logWarning "Not adding roots during realization"
      pure []
    Just rootDir -> getRootFlags rootDir

  -- Call nix-store to evaluate the expression
  let args = ["--realize", unDerivation derivation] ++ rootFlags
      env = [ ("LD_PRELOAD", herculesStoreLibPath)
            , ("NIX_REMOTE", "hercules://")
            ]
      inst = setStdout byteStringOutput
           . setStdin closed
           . setStderr inherit
           . setEnv env
           $ proc (nixPath </> "bin" </> "nix-store") args

  logDebug ("Calling nix-store with " <> T.pack (show args))
  (code, stdout) <- readProcessStdOut inst

  -- Abort on failure
  throwOnFailure "nix-store" code

  -- Return the 'Derivation's in a 'Vector'
  pure . fmap BSL8.unpack . V.fromList . BSL8.lines $ stdout

-- | Throw an error when the exit code is not success
throwOnFailure
  :: MonadThrow m
  => Text
  -- ^ The process name
  -> ExitCode
  -- ^ The exit code
  -> m ()
throwOnFailure processName = \case
  ExitFailure code ->
    throwM $ NixException $ processName <> " failed with code: " <> T.pack (show code)
  ExitSuccess -> pure ()

-- | Get a set of suitable flags for adding an indirect root
getRootFlags :: (MonadLog (WithSeverity Text) m, MonadIO m) => FilePath -> m [String]
getRootFlags rootDir = do
  uniqueDir <- liftIO $ makeAbsolute =<< createTempDirectory rootDir "drvs"
  logDebug ("Created root dir " <> T.pack uniqueDir)
  pure ["--add-root", uniqueDir </> "drv", "--indirect"]


readProcessStdOut
  :: MonadIO m
  => ProcessConfig stdin (STM BSL8.ByteString) stderr -> m (ExitCode, BSL8.ByteString)
readProcessStdOut pc =
  liftIO . withProcess pc $ \p ->
    atomically $ (,) <$> waitExitCodeSTM p <*> getStdout p
