{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
module Control.Applicative.Stacks where

import Data.Functor.Compose
import Control.Applicative
import Control.Applicative.IO
import Control.Applicative.Reader
import Data.Functor.Compose.Where

type family Composing (x :: (* -> *) -> (* -> *) -> * -> *) (fs :: [* -> *]) where
    Composing x (f ': '[]) = f
    Composing x (f ': fs) = x f (Composing x fs)

type Composed = Composing Compose

-- An Example!

-- Note the deriving ApplicativeIO
newtype MyIO a = MyIO (IO a) deriving (Functor, Applicative, ApplicativeIO)

-- We now wrap up MyCompose with a new type, and instance all the Applicative***C classes
newtype MyCompose f g a =
  MyCompose (Compose f g a)
  deriving (Functor,Applicative,ApplicativeIOC OnLeft,ApplicativeIOC OnRight,ApplicativeReaderC OnLeft r,ApplicativeReaderC OnRight r)

-- And provide the top level Applicative*** classes. For IO, note that we use MyIO in the HasApplicativeIO constraint
instance (Applicative g, Applicative f, HasApplicativeIO MyCompose MyIO f g flag) => ApplicativeIO (MyCompose f g) where
  liftAIO = cliftIO (undefined :: flag)

instance (Applicative f,Applicative g,HasApplicativeReader MyCompose (->) r f g flag) => ApplicativeReader r (MyCompose f g) where
  local = local' (undefined :: flag)

type Reader a = (->) a

-- all1 :: (MyCompose (MyCompose ((->) Int) ((->) Float)) MyIO) [Float]
all1 :: Composing MyCompose '[(->) Int, (->) Float, MyIO] [Float]
all1 = liftAIO (putStrLn "Hi") *> (work <$> ask <*> ask)
  where work :: Float -> Int -> [Float]
        work = flip replicate

all2 :: (Composing Compose '[Reader Int, IO]) [Float]
all2 = liftAIO (putStrLn "Hi") *> (work <$> pure (1.0) <*> ask)
  where work :: Float -> Int -> [Float]
        work = flip replicate

-- instance (Applicative f,Applicative g,WhereIs IO (Compose f g) ~ flag,ComposedApplicativeIO flag (Compose f g)) => ApplicativeIO (Compose f g) where
--   liftAIO = cliftIO (undefined :: flag)
