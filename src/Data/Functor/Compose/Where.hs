{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Functor.Compose.Where where

import Data.Functor.Compose

data IsRight
data IsLeft
data Nowhere

type family Or a b where
              Or Nowhere Nowhere = Nowhere
              Or Nowhere f = IsRight
              Or f Nowhere = IsLeft

type family WhereIs x (z :: * -> *) where
  WhereIs x (Compose x f) = IsLeft
  WhereIs x (Compose f x) = IsRight
  WhereIs x (Compose f g) = Or (WhereIs x f) (WhereIs x g)
  WhereIs x f = Nowhere
