{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Nix.Build.Pipe
  ( postponeBuildLines
  , printBuildLines
  , readBuildLines
  ) where

import           Control.Exception (throwIO, try)
import           Control.Monad
import           Data.Semigroup
import           Data.Text         as T
import           Data.Text.IO      as T
import qualified GHC.IO.Exception  as G
import           Pipes
import           Say
import           System.IO
import           Text.Read

import Control.Exception
import System.IO         (Handle)
import System.IO.Error

import Nix.Build.Line

readBuildLines :: Handle -> Producer BuildLine IO ()
readBuildLines h = go
  where
    go = liftIO (hGetLineOrNothing h) >>= \case
      Nothing -> pure ()
      Just l -> do
        case readMaybe (T.unpack l) of
          Nothing -> sayErr ("Failed to parse build line: '" <> l <> "'")
          Just bl -> yield bl
        go

postponeBuildLines :: Producer BuildLine IO ()
postponeBuildLines = do
  eof <- liftIO isEOF
  unless eof $ do
    l <- liftIO T.getLine
    case parseBuildLine l of
      Nothing -> sayErr ("Failed to parse build line: '" <> l <> "'")
      Just bl -> do
        sayErr "# postpone"
        yield bl
    postponeBuildLines

-- | Print the lines to the pipe
printBuildLines :: Handle -> Consumer BuildLine IO ()
printBuildLines pipe = go
  where
    go = do
      bl <- await
      let string = pack (show bl)
      liftIO (try (T.hPutStrLn pipe string >> hFlush pipe)) >>= \case
        -- Gracefully terminate if we got a broken pipe error
        Left e@G.IOError { G.ioe_type = t} ->
            liftIO $ unless (t == G.ResourceVanished) $ throwIO e
        -- Otherwise loop
        Right () -> go

-- | Try to read a line from the handle. If the handle is closed return
-- Nothing. Otherwise behaves the same as 'hGetLine'
hGetLineOrNothing :: Handle -> IO (Maybe Text)
hGetLineOrNothing h = catchJust
  (\e -> if isEOFError e then Just () else Nothing)
  (Just <$> T.hGetLine h)
  (const (pure Nothing))

