{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
module Control.Applicative.Compose where

import Control.Applicative
import Data.Functor.Compose
import Data.Traversable
import Data.Foldable

newtype Inside f g a = Inside {getInside :: Compose f g a} deriving (Functor, Applicative, Foldable, Traversable)
newtype Outside f g a = Outside {getOutside :: Compose f g a} deriving (Functor, Applicative, Foldable, Traversable)
