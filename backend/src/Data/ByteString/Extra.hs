module Data.ByteString.Extra
  ( module Data.ByteString
  , readFileMaybe
  ) where

import Control.Exception
import Data.ByteString

-- | Catch any 'IOException's and return Nothing, otherwise the file contents
readFileMaybe :: FilePath -> IO (Maybe ByteString)
readFileMaybe f = catch (Just <$> Data.ByteString.readFile f) h
  where h :: IOException -> IO (Maybe a)
        h = pure . const Nothing
