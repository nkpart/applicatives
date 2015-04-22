{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Functor.Compose.Where where

data OnRight
data OnLeft
data Nowhere


-- | Find a type within a product of types
--   eg. WhereIs IO Compose (Compose IO Identity) ==> OnLeft
type family WhereIs x (c :: (* -> *) -> (* -> *) -> * -> *) (z :: * -> *) where
  WhereIs x c (c x f) = OnLeft
  WhereIs x c (c f x) = OnRight
  WhereIs x c (c f g) = Or (WhereIs x c f) (WhereIs x c g)
  WhereIs x c f = Nowhere

type family Or a b where
              Or Nowhere Nowhere = Nowhere
              Or Nowhere f = OnRight
              Or f Nowhere = OnLeft
