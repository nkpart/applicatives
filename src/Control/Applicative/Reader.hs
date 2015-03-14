{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-} -- Needed for the recursive type family WhereReader
module Control.Applicative.Reader (
                                  ApplicativeReader(..), ApplicativeReaderC (..), HasApplicativeReader
                                  ) where

import Control.Applicative
import Data.Functor.Compose
import Data.Functor.Compose.Where

-- This is the same as MonadReader
class Applicative f => ApplicativeReader r f  | f -> r where
  ask :: f r
  ask = reader id
  local :: (r -> r) -> f a -> f a
  reader :: (r -> a) -> f a
  reader f = fmap f ask

instance ApplicativeReader r ((->) r) where
  ask = id
  local f m = m . f
  reader = id

-- When instancing for compose, the Reader is either in the left or right branch, and to pick an
-- instance that works for this, we need a type that indicates which branch it is in, otherwise
-- the instances overlap.
-- https://wiki.haskell.org/GHC/AdvancedOverlap#Solution_1_.28using_safer_overlapping_instances.29

class Applicative f => ApplicativeReaderC flag r f  | f -> r where
  ask' :: flag -> f r
  ask' x = reader' x id
  local' :: flag -> (r -> r) -> f a -> f a
  reader' :: flag -> (r -> a) -> f a
  reader' x f = fmap f (ask' x)

instance (Applicative g,ApplicativeReader r f) => ApplicativeReaderC OnLeft r (Compose f g) where
  local' _ f (Compose a) = Compose (local f a)

instance (Applicative f,ApplicativeReader r g) => ApplicativeReaderC OnRight r (Compose f g) where
  local' _ f (Compose a) =
    Compose (fmap (local f) a)

type HasApplicativeReader con re r f g flag = (WhereIs (re r) con (con f g) ~ flag, ApplicativeReaderC flag r (con f g))

instance (Applicative f,Applicative g,HasApplicativeReader Compose (->) r f g flag) => ApplicativeReader r (Compose f g) where
  local = local' (undefined :: flag)
