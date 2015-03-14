{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
module FamilyCompose where

import Control.Applicative

class Applicative f => ApplicativeIO f where
   liftIO :: IO a -> f a

instance ApplicativeIO IO where liftIO = id

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

instance (Applicative f, ApplicativeIO (Compose (fs))) => ComposedApplicativeIO HFalse (Compose (f ': fs)) where
  cliftIO _ = Composed . pure . liftIO


instance (Applicative (Compose (f ': fs)), IsIO f ~ flag , ComposedApplicativeIO flag (Compose (f ': fs))) => ApplicativeIO (Compose (f ': fs)) where
  liftIO = cliftIO (undefined :: flag)
