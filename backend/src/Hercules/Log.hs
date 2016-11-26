{-# LANGUAGE LambdaCase #-}

module Hercules.Log
  ( LogM
  , LogMessage(..)
  , render
  ) where

import Control.Monad.Log
import Data.String
import Data.Text
import Data.Text.Lazy               (fromStrict)
import Text.PrettyPrint.Leijen.Text

-- | See this comment:
-- https://github.com/ocharles/logging-effect/pull/6#issuecomment-252511869 for
-- why this type is necessary.
type LogM l m = LoggingT l (LoggingT (WithTimestamp l) m)

newtype LogMessage = LogString Text

instance IsString LogMessage where
  fromString = LogString . fromString

render :: LogMessage -> Doc
render = \case
  LogString s -> text (fromStrict s)
