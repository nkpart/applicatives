{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Applicative.Stacks where

import Data.Functor.Compose
import Data.Coerce
import Control.Applicative
import Control.Applicative.IO
import Control.Applicative.Reader
import Control.Applicative.Error
import Data.Functor.Compose.Where

-- Some syntax for stacks of some Compose type `c`
type family Composing (c :: (* -> *) -> (* -> *) -> * -> *) (fs :: [* -> *]) where
    Composing c (f ': '[]) = f
    Composing c (f ': fs) = c f (Composing c fs)

type Composed = Composing Compose

-- An Example!

all2 :: Composed '[(->) Int, IO] [Float]
all2 = liftIO (putStrLn "Hi") *> (work <$> pure (1.0) <*> ask)
  where work :: Float -> Int -> [Float]
        work = flip replicate

-- How to Use Your Own IO Type

-- Note the deriving ApplicativeIO
newtype MyIO a = MyIO (IO a) deriving (Functor, Applicative, ApplicativeIO)

-- We now wrap up MyCompose with a new type, and instance all the Applicative***C classes
newtype MyCompose f g a =
  MyCompose (Compose f g a)
  deriving (Functor,Applicative
                    ,ApplicativeIOC OnLeft,ApplicativeIOC OnRight
                    ,ApplicativeReaderC OnLeft r,ApplicativeReaderC OnRight r)

-- And provide the top level Applicative*** classes. For IO, note that we use MyIO in the HasApplicativeIO constraint
instance (HasApplicativeIO MyCompose MyIO f g flag) => ApplicativeIO (MyCompose f g) where
  liftIO = cliftIO (undefined :: flag)

instance (HasApplicativeReader MyCompose (->) r f g flag) => ApplicativeReader r (MyCompose f g) where
  local = local' (undefined :: flag)

-- all1 :: (MyCompose (MyCompose ((->) Int) ((->) Float)) MyIO) [Float]
-- all1 :: Composing MyCompose '[(->) Int, (->) Float, MyIO] [Float]
all1 :: (ApplicativeIO f, ApplicativeReader Float f, ApplicativeReader Int f) => f [Float]
all1 = liftIO (putStrLn "Hi") *> (work <$> ask <*> ask)
  where work :: Float -> Int -> [Float]
        work = flip replicate

runAll1 :: Int -> Float -> MyIO [Float]
runAll1 = coerce (all1 :: Composing MyCompose [(->) Int, (->) Float, MyIO] [Float])

runAll1' :: Float -> Int -> MyIO [Float]
runAll1' = coerce (all1 :: Composing MyCompose [(->) Float, (->) Int, MyIO] [Float])

runAll1'' :: Float -> Int -> IO [Float]
runAll1'' = coerce (all1 :: Composed [(->) Float, (->) Int, IO] [Float])
-- instance (Applicative f,Applicative g,WhereIs IO (Compose f g) ~ flag,ComposedApplicativeIO flag (Compose f g)) => ApplicativeIO (Compose f g) where
--   liftIO = cliftIO (undefined :: flag)



-- | Alternative Stacks to compose

-- | A Compose type that fails with Either
newtype ComposeE f g a = ComposeE (Compose f g a) deriving (Functor, Applicative
                    ,ApplicativeIOC OnLeft,ApplicativeIOC OnRight
                    ,ApplicativeReaderC OnLeft r,ApplicativeReaderC OnRight r
                    ,ApplicativeErrorC OnLeft r,ApplicativeErrorC OnRight r
                                                                                      )
instance (HasApplicativeIO ComposeE IO f g flag) => ApplicativeIO (ComposeE f g) where
  liftIO = cliftIO (undefined :: flag)

instance (HasApplicativeReader ComposeE (->) r f g flag) => ApplicativeReader r (ComposeE f g) where
  local = local' (undefined :: flag)

instance (HasApplicativeError ComposeE Either r f g flag) => ApplicativeError r (ComposeE f g) where
  throwError = throwError' (undefined :: flag)
  catchError = catchError' (undefined :: flag)
