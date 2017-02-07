{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import Data.Foldable
import Data.Monoid         ((<>))
import Data.Text           as T
import Numeric.Natural
import Options.Applicative
import Pipes
import Say
import System.Environment
import System.Exit
import System.IO

import Nix.Build.Pipe

main :: IO ()
main = do
  let pipeVar = "HERCULES_BUILD_HOOK_PIPE"
  pipe <- exitFailureWith (pipeVar <> " not set") =<< lookupEnv (unpack pipeVar)
  withBinaryFile pipe AppendMode runHook
  sayErr "Hook exiting"

runHook :: Handle -> IO ()
runHook pipe = runEffect (postponeBuildLines >-> printBuildLines pipe)

exitFailureWith :: Text -> Maybe a -> IO a
exitFailureWith err = \case
  Nothing -> do
    sayErr err
    exitFailure
  Just x -> pure x

data Config = Config
  { _configLocalSystem   :: Text
  , _configMaxSilentTime :: Natural
  , _configBuildTimeout  :: Natural
  }

options :: ParserInfo Config
options = info (helper <*> parser) description
  where
    parser =
      Config <$>
      (T.pack <$>
       strArgument (fold [metavar "local-system", help "The local system type"])) <*>
      argument
        auto
        (fold
           [ metavar "max-silent-time"
           , help
               "The maximum time in seconds that a builer can go without producing any output on stdout/stderr before it is killed. 0 means infinity."
           ]) <*>
      argument
        auto
        (fold
           [ metavar "build-timeout"
           , help
               "The maximum duration in seconds that a builder can run. 0 means infinity."
           ])
    description =
      fold
        [ fullDesc
        , header "hercules-build-hook"
        , progDesc
            "A program to act as a simple build hook for nix-build, forwarding build requests to a FIFO"
        ]
