{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-} -- Needed for the recursive type family WhereReader

{-# LANGUAGE InstanceSigs #-}
module Control.Applicative.Writer where

import Control.Applicative
import Data.Functor.Compose
import Data.Functor.Compose.Where
import Data.Monoid

type Writer w = (,) w

class (Monoid w, Applicative m) => ApplicativeWriter w m | m -> w where
    writer :: (a,w) -> m a
    writer ~(a, w) = tell w *> pure a

    tell   :: w -> m ()
    tell w = writer ((),w)

    listen :: m a -> m (a, w)

    -- | @'pass' m@ is an action that executes the action @m@, which
    -- returns a value and a function, and returns the value, applying
    -- the function to the output.
    -- pass   :: m (a, w -> w) -> m a
    -- TODO NICK I think instead of pass we have censor, which exists for WriterT
    -- censor :: (Monad m) => (w -> w) -> WriterT w m a -> WriterT w m a
    censor :: (w -> w) -> m a -> m a

instance Monoid w => ApplicativeWriter w ((,) w) where
  listen (w,x) = (w, (x, w))
  writer (a,x) = (x,a)
  censor f (a,x) = (f a, x)

class Applicative f => ApplicativeWriterC flag w f  | f -> w where
    writer' :: flag -> (a,w) -> f a
    tell'   :: flag -> w -> f ()
    listen' :: flag -> f a -> f (a, w)
    censor' :: flag -> (w -> w) -> f a -> f a


instance (Applicative g,ApplicativeWriter w f) => ApplicativeWriterC OnLeft w (Compose f g) where
  writer' _ = Compose . fmap pure . writer
  listen' _ (Compose fga) = Compose $ fmap (\(ga, w) -> fmap (\a -> (a,w)) ga ) $ listen fga
  tell' _ = Compose . fmap pure . tell
  censor' _ f (Compose fga) = Compose $ censor f fga

instance (Applicative f,ApplicativeWriter w g) => ApplicativeWriterC OnRight w (Compose f g) where
  listen' _ (Compose fga) = Compose (fmap listen fga)
  censor' _ f (Compose fga) = Compose (fmap (censor f) fga)
  tell' _ = Compose . pure . tell
  writer' _ = Compose . pure . writer
