{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Functor.Compose.Where where

import Data.Functor.Compose

data OnRight
data OnLeft
data Nowhere

type family Or a b where
              Or Nowhere Nowhere = Nowhere
              Or Nowhere f = OnRight
              Or f Nowhere = OnLeft

type family WhereIs x (c :: (* -> *) -> (* -> *) -> * -> *) (z :: * -> *) where
  WhereIs x c (c x f) = OnLeft
  WhereIs x c (c f x) = OnRight
  WhereIs x c (c f g) = Or (WhereIs x c f) (WhereIs x c g)
  WhereIs x c f = Nowhere
