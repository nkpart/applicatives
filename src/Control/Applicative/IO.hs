{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-} -- Needed for the recursive type family WhereIO
module Control.Applicative.IO
       (ApplicativeIO(..), ComposedApplicativeIO(..)
       ,                   WithIO(..)) where

import Control.Applicative
import Data.Functor.Compose
import Data.Functor.Compose.Where

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

instance (Applicative g, ApplicativeIO f) => ComposedApplicativeIO IsLeft (Compose f g) where
   cliftIO _ = Compose . fmap pure . liftAIO

instance (Applicative f, ApplicativeIO g) => ComposedApplicativeIO IsRight (Compose f g) where
   cliftIO _ = Compose . pure . liftAIO

instance (Applicative f, Applicative g, WhereIs IO (Compose f g) ~ flag , ComposedApplicativeIO flag (Compose f g)) => ApplicativeIO (Compose f g) where
  liftAIO = cliftIO (undefined :: flag)

newtype WithIO (h :: * -> *) f a = WithIO { _unWith :: f a } deriving (Functor, Applicative)

instance (Applicative f,Applicative g,WhereIs h (Compose f g) ~ flag,ComposedApplicativeIO flag (Compose f g)) => ApplicativeIO (WithIO h (Compose f g)) where
  liftAIO z =
    WithIO $
    cliftIO (undefined :: flag) z
