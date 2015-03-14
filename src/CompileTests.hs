{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
module CompileTests where

import           Control.Applicative
import           Control.Applicative.IO
import           Control.Applicative.Reader
import Data.Functor.Compose       as C
import Data.Functor.Compose.Where
import           Data.Functor.Identity

-- | ApplicativeIO instance selection

io6 :: IO e -> (C.Compose IO (C.Compose Identity Identity)) e
io6 = liftIO

io2 :: IO e -> (C.Compose (C.Compose IO Identity) Identity) e
io2 = liftIO

io4 :: IO e -> (C.Compose Identity (C.Compose IO Identity)) e
io4 = liftIO

io1 :: IO e -> (C.Compose (C.Compose Identity IO) Identity) e
io1 = liftIO

io3 :: IO e -> (C.Compose Identity (C.Compose Identity IO)) e
io3 = liftIO

io5 :: IO e -> (C.Compose (C.Compose Identity Identity) IO) e
io5 = liftIO

-- | ApplicativeIO Newtype tests
newtype Watsit a = Watsit (IO a) deriving (Functor, Applicative, ApplicativeIO)

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

-- All the things

all2 :: (Compose ((->) Int) (Compose ((->) String) IO)) [String]
all2 = liftIO (putStrLn "Hi") *> (work <$> ask <*> ask)
  where work :: Int -> String -> [String]
        work = replicate
