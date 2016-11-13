{-# LANGUAGE Arrows          #-}
{-# LANGUAGE RecordWildCards #-}

{-|
A module to handle the different queries we might want to make to Hydra's
database
-}
module Hercules.Query
  ( projectNameQuery
  , projectQuery
  , projectsQuery
  ) where

import Control.Arrow (returnA)
import Data.Text
import Opaleye

import Hercules.Database (Project' (..), ProjectReadColumns, projectTable)

-- | A query to get a list of all the project names
projectNameQuery :: Query (Column PGText)
projectNameQuery = proc () -> do
  Project{..} <- queryTable projectTable -< ()
  returnA -< projectName

-- | A query to get a list of all the projects
projectsQuery :: Query ProjectReadColumns
projectsQuery = proc () -> do
  project@Project{..} <- queryTable projectTable -< ()
  returnA -< project

-- | A query to get all the projects with the specified name (There should be
-- only one)
projectQuery :: Text -> Query ProjectReadColumns
projectQuery name = proc () -> do
  project@Project{..} <- queryTable projectTable -< ()
  restrict -< projectName .== pgStrictText name
  returnA -< project
