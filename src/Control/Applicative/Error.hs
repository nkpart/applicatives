{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-} -- Needed for the recursive type family WhereReader
module Control.Applicative.Error where

import Control.Applicative
import Data.Functor.Compose.Where
import Data.Functor.Compose
import Data.Semigroup
import Data.Validation

class (Applicative f) => ApplicativeError e f | f -> e where
    throwError :: e -> f a
    catchError :: f a -> f (e -> a) -> f a
    -- catchError (throwError e) (pure id) == pure e --- possible law?

-- This is the default ApplicativeError type for a stack of Compose
-- For alternatives, see Control.Applicative.Stacks
instance Semigroup e => ApplicativeError e (AccValidation e) where
  throwError = AccFailure
  catchError (AccFailure e) (AccFailure e2) = AccFailure (e <> e2)
  catchError (AccFailure e) (AccSuccess f) = AccSuccess (f e)
  catchError (AccSuccess _) (AccFailure e) = AccFailure e
  catchError (AccSuccess a) (AccSuccess _) = AccSuccess a

instance ApplicativeError e (Validation e) where
  throwError = Failure
  catchError (Failure e) (Failure _) = Failure e
  catchError (Failure e) (Success f) = Success (f e)
  catchError (Success _) (Failure e) = Failure e
  catchError (Success a) (Success _) = Success a

-- When instancing for compose, the Error is either in the left or right branch, and to pick an
-- instance that works for this, we need a type that indicates which branch it is in, otherwise
-- the instances overlap.
-- https://wiki.haskell.org/GHC/AdvancedOverlap#Solution_1_.28using_safer_overlapping_instances.29

class Applicative f => ApplicativeErrorC flag e f | f -> e where
  throwError' :: flag -> e -> f a
  catchError' :: flag -> f a -> f (e -> a) -> f a

instance (Applicative g, ApplicativeError r f) => ApplicativeErrorC OnLeft r (Compose f g) where
  throwError' _ = Compose . fmap pure . throwError
  catchError' _ (Compose fa) (Compose fge2a) = Compose . catchError fa $ fmap appIn fge2a

instance (Applicative f, ApplicativeError r g) => ApplicativeErrorC OnRight r (Compose f g) where
  throwError' _ = Compose . pure . throwError
  catchError' _ (Compose fa) (Compose fea) = Compose $ liftA2 catchError fa fea

instance (Applicative f, Applicative g, WhereIs (AccValidation e) Compose (Compose f g) ~ flag , ApplicativeErrorC flag e (Compose f g)) => ApplicativeError e (Compose f g) where
  throwError = throwError' (undefined :: flag)
  catchError = catchError' (undefined :: flag)

appIn :: Applicative g => g (e -> a) -> e -> g a
appIn gf e = gf <*> pure e
