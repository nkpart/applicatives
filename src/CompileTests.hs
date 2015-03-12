{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CompileTests where

import           Control.Applicative
import           Control.Applicative.IO
import           Control.Applicative.Reader
import           Control.Applicative.With
import Data.Functor.Compose       as C
import           Data.Functor.Identity

-- | ApplicativeIO instance selection

io6 :: IO e -> (C.Compose IO (C.Compose Identity Identity)) e
io6 = liftAIO

io2 :: IO e -> (C.Compose (C.Compose IO Identity) Identity) e
io2 = liftAIO

io4 :: IO e -> (C.Compose Identity (C.Compose IO Identity)) e
io4 = liftAIO

io1 :: IO e -> (C.Compose (C.Compose Identity IO) Identity) e
io1 = liftAIO

io3 :: IO e -> (C.Compose Identity (C.Compose Identity IO)) e
io3 = liftAIO

io5 :: IO e -> (C.Compose (C.Compose Identity Identity) IO) e
io5 = liftAIO

-- | ApplicativeIO Newtype tests
newtype Watsit a = Watsit (IO a) deriving (Functor, Applicative, ApplicativeIO)

nio1 :: IO e -> WithIO Watsit (Compose Watsit ((->) a)) e
nio1 = liftAIO

-- | ApplicativeReader instance selection

type Reader a = (->) a

r1 :: (C.Compose (Reader a) (C.Compose Identity Identity)) a
r1 = ask

r2 :: (C.Compose (C.Compose (Reader a) Identity) Identity) a
r2 = ask

r3 :: (C.Compose Identity (C.Compose (Reader a) Identity)) a
r3 = ask

r4 :: (C.Compose (C.Compose Identity (Reader a)) Identity) a
r4 = ask

r5 :: (C.Compose Identity (C.Compose Identity (Reader a))) a
r5 = ask

r6 :: (C.Compose (C.Compose Identity Identity) (Reader a)) a
r6 = ask

-- | ApplicativeIO Newtype tests
newtype MyReader a b = MyReader (a -> b) deriving (Functor, Applicative, ApplicativeReader a)

nr1 :: WithReader (MyReader) (Compose (MyReader z) ((->) a)) z
nr1 = ask


xx :: IO () -> (WithIO Watsit (Compose (MyReader Float) Watsit)) ()
xx = liftAIO

all1 :: (WithReader MyReader (Compose (MyReader Int) (WithIO Watsit (Compose (MyReader Float) Watsit)))) [Float]
all1 = liftAIO (putStrLn "Hi") *> (work <$> ask <*> ask)
  where work :: Int -> Float -> [Float]
        work = replicate

all2 :: (Compose ((->) Int) (Compose ((->) String) IO)) [String]
all2 = liftAIO (putStrLn "Hi") *> (work <$> ask <*> ask)
  where work :: Int -> String -> [String]
        work = replicate
