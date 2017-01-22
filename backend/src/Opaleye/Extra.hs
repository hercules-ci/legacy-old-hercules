module Opaleye.Extra
  ( module Opaleye
  , eqNullable
  ) where

import Opaleye

-- | Returns false when the second input is nullable
eqNullable :: Column a -> Column (Nullable a) -> Column PGBool
eqNullable a = matchNullable (pgBool False) (a .==)
