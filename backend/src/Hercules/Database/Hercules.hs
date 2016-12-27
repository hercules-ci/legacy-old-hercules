{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Hercules.Database.Hercules where

import Data.Profunctor
import Data.Profunctor.Product
import Data.Profunctor.Product.Default
import Data.Profunctor.Product.TH      (makeAdaptorAndInstance)
import Data.Text
import GHC.Int
import Opaleye                         hiding (fromNullable)

-- | A newtype around @a -> Maybe b@ to facilitate conversions from the
-- Nullable types.
newtype ToMaybe a b = ToMaybe { unToMaybe :: a -> Maybe b }

instance Profunctor ToMaybe where
  dimap f g (ToMaybe h) = ToMaybe (fmap g . h . f)

instance ProductProfunctor ToMaybe where
  empty = ToMaybe pure
  (ToMaybe f) ***! (ToMaybe g) = ToMaybe (\(x, y) -> (,) <$> f x <*> g y)

-- | This instance makes sure that values which are required in the output are
-- required in the input.
instance Default ToMaybe (Maybe a) a where
  def = ToMaybe id

-- | This instance allows values which are optional in the output to be
-- optional in the input.
instance Default ToMaybe (Maybe a) (Maybe a) where
  def = ToMaybe pure

-- | Convert from any Nullable type by "sequencing" over all the fields.
fromNullable :: Default ToMaybe a b => a -> Maybe b
fromNullable = unToMaybe def

---- Types for table: users ----

data User' c1 c2 c3 c4 =
  User
    { userId       :: c1
    , userName     :: c2
    , userEmail    :: c3
    , userGithubId :: c4
    }

type User = User' Int64 (Maybe Text) (Maybe Text) (Maybe Text)

type UserReadColumns = User' (Column PGInt8) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText))

type UserWriteColumns = User' (Maybe (Column PGInt8)) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText)))

type UserNullableColumns = User' (Column (Nullable PGInt8)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText))

type UserNullable = User' (Maybe Int64) (Maybe Text) (Maybe Text) (Maybe Text)

fromNullableUser :: UserNullable -> Maybe User
fromNullableUser = fromNullable

$(makeAdaptorAndInstance "pUser" ''User')

userTable :: Table UserWriteColumns UserReadColumns
userTable = Table "users" (pUser
  User
    { userId = optional "id"
    , userName = optional "name"
    , userEmail = optional "email"
    , userGithubId = optional "github_id"
    }
  )

