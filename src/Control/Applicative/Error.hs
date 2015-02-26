{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}
module Control.Applicative.Error where

import Control.Applicative
import Control.Applicative.Compose
import Data.Semigroup
import Data.Functor.Compose
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

instance (Applicative g, ApplicativeError e f) => ApplicativeError e (Outside f g) where
  throwError = Outside . fmap pure . throwError
  catchError (Outside fa) (Outside fge2a) = Outside . catchError fa $ fmap appIn fge2a

appIn :: Applicative g => g (e -> a) -> e -> g a
appIn gf e = gf <*> pure e

instance (Applicative f, ApplicativeError e g) => ApplicativeError e (Inside f g) where
  throwError = Inside . pure . throwError
  catchError (Inside fa) (Inside fea) = Inside $ liftA2 catchError fa fea




instance (Applicative f, ApplicativeError e g) => ApplicativeError e (Compose f g) where
  throwError = Compose . pure . throwError
  catchError (Compose fa) (Compose fea) = Compose $ liftA2 catchError fa fea
