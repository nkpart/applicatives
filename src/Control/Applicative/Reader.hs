{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}
module Control.Applicative.Reader where

import Control.Applicative
import Data.Functor.Compose

-- This is the same as MonadReader

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

newtype InnerReader r f a = InnerReader {getInnerReader :: f (r -> a) }

instance (Applicative f) => Applicative (InnerReader r f)
instance (Functor f) => Functor (InnerReader r f)

instance (Applicative f) => ApplicativeReader r (InnerReader r f) where
  ask = InnerReader . pure $ ask
  local f (InnerReader fga) = InnerReader $ fmap (local f) fga
  reader = InnerReader . pure . reader

newtype OuterReader r f a = OuterReader {getOuterReader :: r -> f a }

instance (Applicative f) => Applicative (OuterReader r f)
instance (Functor f) => Functor (OuterReader r f)

instance (Applicative f) => ApplicativeReader r (OuterReader r f) where
  ask = OuterReader . fmap pure $ ask
  local f (OuterReader fga) = OuterReader $ local f fga
  reader = OuterReader . fmap pure . reader
