module Control.Applicative.IO where

import Control.Applicative
import Control.Applicative.Compose
import Data.Functor.Compose

class Applicative f => ApplicativeIO f where
   liftAIO :: IO a -> f a

instance ApplicativeIO IO where
  liftAIO = id

instance (Applicative g, ApplicativeIO f) => ApplicativeIO (Outside f g) where
  liftAIO = Outside . Compose . fmap pure . liftAIO

instance (Applicative f, ApplicativeIO g) => ApplicativeIO (Inside f g) where
  liftAIO = Inside . Compose . pure . liftAIO
