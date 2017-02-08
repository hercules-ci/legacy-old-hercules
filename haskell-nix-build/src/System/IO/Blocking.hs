module System.IO.Blocking
  ( withFileBlocking
  ) where

import Control.Exception (bracket)
import GHC.IO.Handle.FD
import System.IO

withFileBlocking :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFileBlocking name mode = bracket (openFileBlocking name mode) hClose
