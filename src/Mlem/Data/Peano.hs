{-# LANGUAGE UndecidableInstances #-}
-- |

module Mlem.Data.Peano
  ( Peano(..)
  , PN
  ) where

import Mlem.Prelude

data Peano where
  Z :: Peano
  S :: Peano -> Peano

type family PN (n :: Nat) :: Peano where
  PN 0 = Z
  PN n = S (PN (n - 1))
