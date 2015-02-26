{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}
module Control.Applicative.Reader where

import Control.Applicative
import Control.Applicative.Compose
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

instance (Applicative g, ApplicativeReader r f) => ApplicativeReader r (Outside f g) where
  ask = Outside . fmap pure $ ask
  local f (Outside fga) = Outside $ local f fga
  reader = Outside . fmap pure . reader

instance (Applicative g, ApplicativeReader r f) => ApplicativeReader r (Compose f g) where
  ask = Compose . fmap pure $ ask
  local f (Compose fga) = Compose $ local f fga
  reader = Compose . fmap pure . reader

instance (Applicative f, ApplicativeReader r g) => ApplicativeReader r (Inside f g) where
  ask = Inside . pure $ ask
  local f (Inside fga) = Inside $ fmap (local f) fga
  reader = Inside . pure . reader
