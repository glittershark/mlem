{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
--------------------------------------------------------------------------------
module Mlem.Data.Vec
  ( Vec(..)
  , replicate
  ) where
--------------------------------------------------------------------------------
import Mlem.Prelude hiding
  ( zipWith
  , replicate
  )
import Data.Constraint
--------------------------------------------------------------------------------
import Mlem.Data.Peano
import Mlem.Constraint.Axioms
--------------------------------------------------------------------------------

infixl 4 :<

data Vec (n :: Nat) (a :: Type) where
  VNil :: Vec 0 a
  (:<) :: a -> Vec n a -> Vec (n + 1) a

instance Functor (Vec n) where
  fmap _ VNil      = VNil
  fmap f (x :< xs) = f x :< fmap f xs

class Replicate (n :: Nat) (n' :: Peano) where
  replicate' :: a -> Vec n a

instance Replicate 0 Z where
  replicate' _ = VNil

instance (Replicate n n', m ~ (n + 1)) => Replicate m (S n') where
  replicate' x = x :< replicate' @n @n' x

replicate :: forall n a. Replicate n (PN n) => a -> Vec n a
replicate = replicate' @n @(PN n)

zipWith :: forall n a b c. (a -> b -> c) -> Vec n a -> Vec n b -> Vec n c
zipWith _ VNil VNil = VNil
zipWith f (a :< (as :: Vec n1 a)) (b :< (bs :: Vec m1 b)) =
  f a b :< case plusInj @n1 @m1 @n @1 of
    Sub Dict -> zipWith f as bs

-- | Acts like a ZipList
instance Replicate n (PN n) => Applicative (Vec n) where
  pure  = replicate @n
  (<*>) = zipWith id
