{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}
module Control.Applicative.Error where

import Control.Applicative
import Control.Applicative.Compose
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

instance (Applicative g, ApplicativeError e f) => ApplicativeError e (Outside f g) where
  throwError = Outside . Compose . fmap pure . throwError
  catchError (Outside (Compose fa)) (Outside (Compose fge2a)) = Outside . Compose $ catchError fa $ fmap appIn fge2a

appIn :: Applicative g => g (e -> a) -> e -> g a
appIn gf e = gf <*> pure e

-- If you have AppError on inside of Compose, you have AppError for compose
instance (Applicative f, ApplicativeError e g) => ApplicativeError e (Inside f g) where
  throwError = Inside . Compose . pure . throwError
  catchError (Inside (Compose fa)) (Inside (Compose fea)) = Inside . Compose $ liftA2 catchError fa fea