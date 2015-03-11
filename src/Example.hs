{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
module Example where

import Control.Applicative.IO
import Control.Applicative
import Data.Validation
import Data.Functor.Compose
import Control.Applicative.Error
import Control.Applicative.Reader

big :: ApplicativeError [String] f => Int -> f Int
big n | n < 10 = throwError ["sorry dibblego"]
      | otherwise = pure n

vOdd n | even n  = throwError ["its even"]
       | otherwise = (+ n) <$> ask

recover :: Num a => t -> a
recover _ = 0

type Reader a = (->) a

ex1 :: (ApplicativeError [String] f, ApplicativeReader Int f) => Int -> Int -> f Int
ex1 a b = (+) <$> big a <*> vOdd b

-- ex1T :: Int -> Int -> (Int `OuterReader` (IO `Compose` AccValidation [String])) Int
-- ex1T = ex1

-- ex1Recovered :: Int -> Int -> (Int `OuterReader` (OuterIO (AccValidation [String]))) Int
-- ex1Recovered a b = ex1 a b `catchError` pure recover

-- f :: IO c -> (a -> IO (b -> c))
-- f = liftAIO

type W a b c d e = (Reader a `Compose` Reader b `Compose` (IO `Compose` (Reader c `Compose` Reader d))) e

fg :: (a -> b -> IO (c -> d -> e)) -> W a b c d e
fg = Compose . Compose . (fmap . fmap) (Compose . fmap Compose)

fg' :: W a b c d e -> (a -> b -> IO (c -> d -> e))
fg' = undefined

-- ff :: IO e -> W a b c d e
-- ff = liftAIO

-- fa :: W a b c d a
-- fa = ask

-- fb :: W a b c d b
-- fb = ask

-- fc :: W a b c d c
-- fc = ask

-- fd :: W a b c d d
-- fd = ask
