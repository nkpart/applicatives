module CompileTests where

import           Control.Applicative.IO
import           Control.Applicative.Reader
import qualified Data.Functor.Compose   as C
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

-- | ApplicativeReader instance selection

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
