{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-} -- Needed for the recursive type family WhereIO
module Control.Applicative.IO where

import Control.Applicative
import Data.Functor.Compose

class Applicative f => ApplicativeIO f where
   liftAIO :: IO a -> f a

instance ApplicativeIO IO where
  liftAIO = id

-- When instancing for compose, the IO is either in the left or right branch, and to pick an
-- instance that works for this, we need a type that indicates which branch it is in, otherwise
-- the instances overlap.
-- https://wiki.haskell.org/GHC/AdvancedOverlap#Solution_1_.28using_safer_overlapping_instances.29

class ComposedApplicativeIO flag f where
   cliftIO :: flag -> IO a -> f a

data IsRight
data IsLeft
data Nowhere

type family Or a b where
              Or Nowhere Nowhere = Nowhere
              Or Nowhere f = IsRight
              Or f Nowhere = IsLeft

type family WhereIO z where
  WhereIO (Compose IO f) = IsLeft
  WhereIO (Compose f IO) = IsRight
  WhereIO (Compose f g) = Or (WhereIO f) (WhereIO g)
  WhereIO f = Nowhere

instance (Applicative g, ApplicativeIO f) => ComposedApplicativeIO IsLeft (Compose f g) where
   cliftIO _ = Compose . fmap pure . liftAIO

instance (Applicative f, ApplicativeIO g) => ComposedApplicativeIO IsRight (Compose f g) where
   cliftIO _ = Compose . pure . liftAIO

instance (Applicative f, Applicative g, WhereIO (Compose f g) ~ flag , ComposedApplicativeIO flag (Compose f g)) => ApplicativeIO (Compose f g) where
  liftAIO = cliftIO (undefined :: flag)

-- For demonstration, these are the instances that overlap

-- instance (Applicative (Compose fs)) => ApplicativeIO (Compose (IO ': fs)) where
--   liftAIO x = Composed . fmap pure $ x

-- instance (Applicative f, Applicative (Compose fs), ApplicativeIO (Compose fs)) => ApplicativeIO (Compose (f ': fs)) where
--   liftAIO x = Composed . pure . liftAIO $ x
