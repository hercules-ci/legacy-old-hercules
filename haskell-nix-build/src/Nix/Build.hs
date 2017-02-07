{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Nix.Build
  ( evaluate
  -- , build
  ) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.ByteString.Lazy      (toStrict)
import Data.Foldable
import Data.IORef
import Data.List.Extra           as List (foldl1', nubOrdOn)
import Data.Semigroup
import Data.Text                 as T
import Data.Text.Encoding
import Paths_haskell_nix_build
import Pipes
import System.Directory
import System.Environment        (getEnvironment, lookupEnv)
import System.Exit
import System.FilePath
import System.IO
import System.IO.Error
import System.IO.Temp
import System.Posix.Files
import System.Process.Typed

import Nix.Build.Pipe

newtype Derivation = Derivation { unDerivation :: FilePath }
  deriving (Show)

data BuildRequirements a
  = NeedDerivation Derivation
    -- ^ This derivation needs to be present in the store to continue
  | Built a

-- | 'evalutate' takes a path to a .nix file and returns the list of
-- derivations asynchronously, it also returns an 'IO' action which will return
-- all the derivations which are being build during evaluation.
evaluate
  :: (MonadIO m, MonadError Text m)
  => FilePath -> m (IO [Derivation], Async (Either Text [Derivation]))
evaluate nix = do
  needed <- liftIO $ newIORef []
  let pushNeeded drvs = modifyIORef' needed (<> drvs)
  liftIO findBuildHook >>= \case
    Nothing -> throwError "Can't find 'hercules-build-hook'"
    Just hook -> do
      builder <- liftIO (async (evalutate' hook pushNeeded nix))
      pure (readIORef needed, builder)

evalutate'
  :: FilePath
  -- ^ The path to hercules-build-hook
  -> ([Derivation] -> IO ())
  -- ^ A function to push the names of derivations being built
  -> FilePath
  -- ^ The .nix file to evaluate
  -> IO (Either Text [Derivation])
  -- ^ The list of derivations in this expression
evalutate' hook needed nix =
  withSystemTempDirectory "hercules-build-hook" $ \tmp -> do
    let pipe = tmp </> "pipe"
    createNamedPipe
      pipe
      (List.foldl1' unionFileModes
                    [ namedPipeMode
                    , ownerReadMode
                    , ownerWriteMode
                    ])
    (r, _) <- concurrently (runInstantiate hook pipe nix) (servicePipe pipe needed)
    pure r

runInstantiate
  :: FilePath
  -- ^ The path to hercules-build-hook
  -> FilePath
  -- ^ The path the the pipe for the build hook to write to
  -> FilePath
  -- ^ The nix expression to instantiate
  -> IO (Either Text [Derivation])
  -- ^ The list of derivations in this expression
runInstantiate hook pipe nix = do
  env <- getEnvironment
  let envOverrides = [ ("NIX_BUILD_HOOK", hook)
                     , ("HERCULES_BUILD_HOOK_PIPE", pipe)
                     , ("LOCALE", "C")
                     ]
  let newEnv = nubOrdOn fst (envOverrides <> env)
  let pc = setEnv newEnv
         . setStdout byteStringOutput
         . setStdin closed
         $ proc "nix-instantiate" [nix]
  (exitCode, stdout) <-
    withProcess pc (\p -> (,) <$> waitExitCode p <*> atomically (getStdout p))
  case exitCode of
    ExitFailure _ -> pure $ Left "nix-instantiate failed"
    ExitSuccess   -> pure $ Right $
      let ls = T.lines (decodeUtf8 . toStrict $ stdout)
      in Derivation . T.unpack <$> ls

servicePipe
  :: FilePath
  -- ^ The path to the fifo to read from
  -> ([Derivation] -> IO ())
  -- ^ An action to run when a new derivation is requested on the pipe
  -> IO ()
servicePipe pipe needed = withFile pipe ReadMode $ \h ->
  runEffect (readBuildLines h >-> printBuildLines stdout)

-- build :: MonadIO m => Derivation ->
build = undefined

-- | Try and find the 'hercules-build-hook' binary.
--
-- If the @HERCULES_BUILD_HOOK@ env variable is set to an executable binary
-- then use that.
--
-- Then look in the install directory.
--
-- Otherwise look in @PATH@ for @hercules-build-hook@
findBuildHook :: IO (Maybe FilePath)
findBuildHook = runMaybeT (asum [lookInEnv, lookInInstall, lookInPath])
  where
    binName = "hercules-build-hook"
    lookInEnv :: MaybeT IO FilePath
    lookInEnv = do
      h <- MaybeT (lookupEnv "HERCULES_BUILD_HOOK")
      guard =<< liftIO (existsAndIsExecutable h)
      pure h
    lookInInstall :: MaybeT IO FilePath
    lookInInstall = do
      binDir <- liftIO getBinDir
      let h = binDir </> binName
      guard =<< liftIO (existsAndIsExecutable h)
      pure h
    lookInPath :: MaybeT IO FilePath
    lookInPath = MaybeT (findExecutable binName)

-- | Return 'True' iff a file exists and is executable
existsAndIsExecutable :: FilePath -> IO Bool
existsAndIsExecutable file = isExecutable file `catchIOError` const (pure False)

-- | Test whether a file is executable.
isExecutable :: FilePath -> IO Bool
isExecutable file = do
    perms <- getPermissions file
    return (executable perms)

