{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
module Example where

import Control.Applicative
import Data.Validation
import Data.Functor.Compose
import Control.Applicative.Error
import Control.Applicative.Reader

big :: ApplicativeError [String] f => Int -> f Int
big n | n < 10 = throwError ["sorry dibblego"]
      | otherwise = pure n

vOdd :: (ApplicativeError [String] f, ApplicativeReader a f, Integral a) => a -> f a
vOdd n | even n  = throwError ["its even"]
       | otherwise = (+ n) <$> ask

recover :: Num a => t -> a
recover _ = 0

type Reader a = (->) a

ex1 :: (ApplicativeError [String] f, ApplicativeReader Int f) => Int -> Int -> f Int
ex1 a b = (+) <$> big a <*> vOdd b

ex1T :: Int -> Int -> (Reader Int `Compose` (IO `Compose` AccValidation [String])) Int
ex1T = ex1

ex1Recovered :: Int -> Int -> (Reader Int `Compose` (IO `Compose` AccValidation [String])) Int
ex1Recovered a b = ex1 a b `catchError` pure recover
