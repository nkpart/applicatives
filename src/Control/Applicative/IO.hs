{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
module Control.Applicative.IO where

import Control.Applicative
import qualified Data.Functor.Compose as C

class Applicative f => ApplicativeIO f where
   liftAIO :: IO a -> f a

instance ApplicativeIO IO where
  liftAIO = id

-- This doesn't work because type synonyms
type family ComposeT fs x where
    ComposeT '[] a = a
    ComposeT (f ': fs) a = (f (Compose fs a))

-- 1. This compose models the composition as a list of type constructors, rather than the tree

data family Compose :: [* -> *] -> * -> *
data instance Compose '[] a = Id a
data instance Compose (f ': fs) a = Composed (f (Compose fs a))

instance Functor (Compose '[]) where
  fmap f (Id a) = Id (f a)

instance (Functor f, Functor (Compose fs)) => Functor (Compose (f ': fs)) where
  fmap f (Composed a) = Composed (fmap (fmap f) a)

instance Applicative (Compose '[]) where
  pure a = Id a
  Id f <*> Id a = Id (f a)

instance (Applicative f, Applicative (Compose fs)) => Applicative (Compose (f ': fs)) where
  pure a = Composed (pure . pure $ a)
  Composed f <*> Composed a = Composed $ (<*>) <$> f <*> a

-- NOW TO INSTANCE IT FOR APP IO!

data HFalse
data HTrue

type family IsIO z where
            IsIO IO = HTrue
            IsIO f = HFalse

class ComposedApplicativeIO flag f where
   cliftIO :: flag -> IO a -> f a

instance Applicative (Compose fs) => ComposedApplicativeIO HTrue (Compose (IO ': fs)) where
   cliftIO _ = Composed . fmap pure

-- instance (Applicative f, IsIO g ~ z, ComposedApplicativeIO z (Compose (g ': fs))) => ComposedApplicativeIO HFalse (Compose (f ': g ': fs)) where
--   cliftIO _ = Composed . pure . cliftIO (undefined :: z)


instance (Applicative f, ApplicativeIO (Compose (fs))) => ComposedApplicativeIO HFalse (Compose (f ': fs)) where
  cliftIO _ = Composed . pure . liftAIO


instance (Applicative (Compose (f ': fs)), IsIO f ~ flag , ComposedApplicativeIO flag (Compose (f ': fs))) => ApplicativeIO (Compose (f ': fs)) where
  liftAIO = cliftIO (undefined :: flag)


data IsLeft
data Nope

-- https://wiki.haskell.org/GHC/AdvancedOverlap#Solution_1_.28using_safer_overlapping_instances.29

-- type family Or a b where
--               Or Nope f = Nope
--               Or f Nope = IsLeft

type family WhereIO z where
  WhereIO (C.Compose IO f) = IsLeft
  -- WhereIO (C.Compose f IO) = Nope
  -- WhereIO (C.Compose f g) = Or (WhereIO f) (WhereIO g)
  WhereIO f = Nope

instance (Applicative g, ApplicativeIO f) => ComposedApplicativeIO IsLeft (C.Compose f g) where
instance (Applicative f, ApplicativeIO g) => ComposedApplicativeIO Nope (C.Compose f g) where
-- instance (Applicative g, ApplicativeIO (C.Compose p q)) => ComposedApplicativeIO IsLeftL (C.Compose (C.Compose p q) g) where
-- instance (Applicative f, ApplicativeIO (C.Compose p q)) => ComposedApplicativeIO IsRightL (C.Compose f (C.Compose p q)) where
instance (Applicative f, Applicative g, WhereIO (C.Compose f g) ~ flag , ComposedApplicativeIO flag (C.Compose f g)) => ApplicativeIO (C.Compose f g) where

-- This is bad stuff that has overlaps, the type family isIO defeats this
-- With overlaps

-- instance (Applicative (Compose fs)) => ApplicativeIO (Compose (IO ': fs)) where
--   liftAIO x = Composed . fmap pure $ x

-- instance (Applicative f, Applicative (Compose fs), ApplicativeIO (Compose fs)) => ApplicativeIO (Compose (f ': fs)) where
--   liftAIO x = Composed . pure . liftAIO $ x

-- THE AWESOME EXAMPLE
type Reader = (->)

-- type W a b c d = Compose '[Reader a, Reader b, IO, Reader c, Reader d] -- (Reader a `Composed` Reader b `Composed` (IO `Composed` (Reader c `Composed` Reader d))) e
type W a b c d e = (Reader a `C.Compose` Reader b `C.Compose` (IO `C.Compose` (Reader c `C.Compose` Reader d))) e
type W2 a b c d e = (Reader a `C.Compose` Reader b `C.Compose` (Reader c `C.Compose` (IO `C.Compose` Reader d))) e

fg :: (a -> b -> IO (c -> d -> e)) -> W a b c d e
fg = undefined

fg' :: W a b c d e -> (a -> b -> IO (c -> d -> e))
fg' = undefined

ff :: IO e -> W a b c d e
ff = liftAIO

ff2 :: IO e -> W2 a b c d e
ff2 = liftAIO
