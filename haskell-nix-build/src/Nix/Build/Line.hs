{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Nix.Build.Line
  ( BuildLine(..)
  , parseBuildLine
  ) where

import Data.Text as T

data BuildLine = BuildLine
  { blAmWilling        :: Bool
  , blNeededSystem     :: Text
  , blDrvPath          :: FilePath
  , blRequiredFeatures :: [Text]
  }
  deriving (Show)

parseBuildLine :: Text -> Maybe BuildLine
parseBuildLine t = case T.words t of
  amWilling : neededSystem : drvPath : l -> do
    blAmWilling <- case amWilling of
      "0" -> Just True
      "1" -> Just False
      _   -> Nothing
    let blNeededSystem = neededSystem
        blDrvPath = T.unpack drvPath
    blRequiredFeatures <- case l of
      []                 -> Just []
      [requiredFeatures] -> Just (splitOn "," requiredFeatures)
      _                  -> Nothing
    pure BuildLine{..}

  _ -> Nothing
