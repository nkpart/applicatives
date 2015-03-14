{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-} -- Needed for the recursive type family WhereIO
{-# LANGUAGE ConstraintKinds #-} -- Needed for the recursive type family WhereIO
module Control.Applicative.IO
       (ApplicativeIO(..), ApplicativeIOC(..), HasApplicativeIO
 ) where

import Control.Applicative
import Data.Functor.Compose
import Data.Functor.Compose.Where

class Applicative f => ApplicativeIO f where
   liftAIO :: IO a -> f a

instance ApplicativeIO IO where
  liftAIO = id

-- When instancing for compose, the IO is either in the left or right branch, and to pick an
-- instance that works for this, we need a type that indicates which branch it is in, otherwise
-- the instances overlap.
-- https://wiki.haskell.org/GHC/AdvancedOverlap#Solution_1_.28using_safer_overlapping_instances.29

class ApplicativeIOC flag f where
   cliftIO :: flag -> IO a -> f a

instance (Applicative g, ApplicativeIO f) => ApplicativeIOC OnLeft (Compose f g) where
   cliftIO _ = Compose . fmap pure . liftAIO

instance (Applicative f, ApplicativeIO g) => ApplicativeIOC OnRight (Compose f g) where
   cliftIO _ = Compose . pure . liftAIO

-- The constraints required to find a particular IO type in a stack of composes
-- The first 2 parameters should be specialised for the ApplicativeIO instance of a particular compose type,
-- where `con` is the wrapper around compose you are providing an instance for, and `io` is the type that has
-- an ApplicativeIO instance.
type HasApplicativeIO con io f g flag = (WhereIs io con (con f g) ~ flag,ApplicativeIOC flag (con f g), Applicative f, Applicative g)

instance (Applicative f,Applicative g,HasApplicativeIO Compose IO f g flag) => ApplicativeIO (Compose f g) where
  liftAIO = cliftIO (undefined :: flag)
