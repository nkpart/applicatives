{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
module Control.Applicative.Compose where

import Control.Applicative
import Data.Functor.Compose
import Data.Traversable
import Data.Foldable

newtype Inside f g a =
  Inside {getInside :: f (g a)}
  deriving (Functor,Foldable,Traversable)

instance (Applicative f, Applicative g) => Applicative (Inside f g) where
  pure = Inside . pure . pure
  Inside f <*> Inside a = Inside . getCompose $ Compose f <*> Compose a

newtype Outside f g a =
  Outside {getOutside :: f (g a)}
  deriving (Functor,Foldable,Traversable)

instance (Applicative f, Applicative g) => Applicative (Outside f g) where
  pure = Outside . pure . pure
  Outside f <*> Outside a = Outside . getCompose $ Compose f <*> Compose a
