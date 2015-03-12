{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Control.Applicative.With where

import Data.Functor.Compose
import Data.Functor.Compose.Where
import Control.Applicative.Reader
import Control.Applicative.IO
import Control.Applicative

newtype WithReader (h :: * -> * -> *) f a = WithReader { unWithReader :: f a } deriving (Functor, Applicative, ApplicativeIO)

instance (Applicative f, Applicative g, WhereIs (h r) (Compose f g) ~ flag , ComposedApplicativeReader flag r (Compose f g)) => ApplicativeReader r (WithReader h (Compose f g)) where
  local f (WithReader h) = WithReader $ local' (undefined :: flag) f h



-- instance ApplicativeReader r f => ApplicativeReader r (WithIO h f) where
--   local f (WithIO a) =
--     WithIO $
--     local f a
