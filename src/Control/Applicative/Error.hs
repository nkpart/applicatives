{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-} -- Needed for the recursive type family WhereReader
module Control.Applicative.Error where

import Control.Applicative
import Data.Functor.Compose
import Data.Semigroup
import Data.Validation

class (Applicative f) => ApplicativeError e f | f -> e where
    throwError :: e -> f a
    catchError :: f a -> f (e -> a) -> f a
    -- catchError (throwError e) (pure id) == pure e --- possible law?

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

class Applicative f => ComposedApplicativeError flag e f | f -> e where
  throwError' :: flag -> e -> f a
  catchError' :: flag -> f a -> f (e -> a) -> f a

data IsRight
data IsLeft
data Nowhere

type family Or a b where
              Or Nowhere Nowhere = Nowhere
              Or Nowhere f = IsRight
              Or f Nowhere = IsLeft

type family WhereError c e z where
  WhereError (ApplicativeError e g) e (Compose g f) = IsLeft
  -- WhereError e (Compose g f) = IsRight
  -- WhereError e (Compose f g) = Or (WhereError e f) (WhereError e g)
  -- WhereError e f = Nowhere

instance (Applicative g, ApplicativeError r f) => ComposedApplicativeError IsLeft r (Compose f g) where

instance (Applicative f, ApplicativeError r g) => ComposedApplicativeError IsRight r (Compose f g) where

instance (Applicative f, Applicative g, WhereError r (Compose f g) ~ flag , ComposedApplicativeError flag r (Compose f g)) => ApplicativeError r (Compose f g) where

-- instance (Applicative g, ApplicativeError e f) => ApplicativeError e (Outside f g) where
--   throwError = Outside . fmap pure . throwError
--   catchError (Outside fa) (Outside fge2a) = Outside . catchError fa $ fmap appIn fge2a

-- appIn :: Applicative g => g (e -> a) -> e -> g a
-- appIn gf e = gf <*> pure e

-- instance (Applicative f, ApplicativeError e g) => ApplicativeError e (Inside f g) where
--   throwError = Inside . pure . throwError
--   catchError (Inside fa) (Inside fea) = Inside $ liftA2 catchError fa fea


-- instance (Applicative f, ApplicativeError e g) => ApplicativeError e (Compose f g) where
--   throwError = Compose . pure . throwError
--   catchError (Compose fa) (Compose fea) = Compose $ liftA2 catchError fa fea
