{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-} -- Needed for the recursive type family WhereReader
module Control.Applicative.Reader where

import Control.Applicative
import Data.Functor.Compose

-- This is the same as MonadReader

type Reader a = (->) a

class Applicative f => ApplicativeReader r f | f -> r where
    ask   :: f r
    ask = reader id

    local :: (r -> r) -> f a -> f a

    reader :: (r -> a) -> f a
    reader f = fmap f ask

instance ApplicativeReader r ((->) r) where
  ask = id
  local f m = m .f
  reader = id

-- When instancing for compose, the Reader is either in the left or right branch, and to pick an
-- instance that works for this, we need a type that indicates which branch it is in, otherwise
-- the instances overlap.
-- https://wiki.haskell.org/GHC/AdvancedOverlap#Solution_1_.28using_safer_overlapping_instances.29

class Applicative f => ComposedApplicativeReader flag r f | f -> r where
   ask'   :: flag -> f r
   ask' x = reader' x id

   local' :: flag -> (r -> r) -> f a -> f a

   reader' :: flag -> (r -> a) -> f a
   reader' x f = fmap f (ask' x)

data IsRight
data IsLeft
data Nowhere

type family Or a b where
              Or Nowhere Nowhere = Nowhere
              Or Nowhere f = IsRight
              Or f Nowhere = IsLeft

type family WhereReader a z where
  WhereReader a (Compose (Reader a) f) = IsLeft
  WhereReader a (Compose f (Reader a)) = IsRight
  WhereReader a (Compose f g) = Or (WhereReader a f) (WhereReader a g)
  WhereReader a f = Nowhere

instance (Applicative g, ApplicativeReader r f) => ComposedApplicativeReader IsLeft r (Compose f g) where
  local' _ f (Compose a) = Compose (local f a)

instance (Applicative f, ApplicativeReader r g) => ComposedApplicativeReader IsRight r (Compose f g) where
  local' _ f (Compose a) = Compose (fmap (local f) a)

instance (Applicative f, Applicative g, WhereReader r (Compose f g) ~ flag , ComposedApplicativeReader flag r (Compose f g)) => ApplicativeReader r (Compose f g) where
  local = local' (undefined :: flag)

-- newtype InnerReader r f a = InnerReader {getInnerReader :: f (r -> a) }

-- instance (Applicative f) => Applicative (InnerReader r f)
-- instance (Functor f) => Functor (InnerReader r f)

-- instance (Applicative f) => ApplicativeReader r (InnerReader r f) where
--   ask = InnerReader . pure $ ask
--   local f (InnerReader fga) = InnerReader $ fmap (local f) fga
--   reader = InnerReader . pure . reader

-- newtype OuterReader r f a = OuterReader {getOuterReader :: r -> f a }

-- instance (Applicative f) => Applicative (OuterReader r f)
-- instance (Functor f) => Functor (OuterReader r f)

-- instance (Applicative f) => ApplicativeReader r (OuterReader r f) where
--   ask = OuterReader . fmap pure $ ask
--   local f (OuterReader fga) = OuterReader $ local f fga
--   reader = OuterReader . fmap pure . reader
