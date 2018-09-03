{-# LANGUAGE AllowAmbiguousTypes #-}
-- |

module Mlem.Constraint.Axioms
  ( plusInj
  ) where

import Mlem.Prelude
import Data.Constraint
import Unsafe.Coerce

axiom :: forall a b. Dict (a ~ b)
axiom = unsafeCoerce (Dict @(a ~ a))

plusInj :: forall n m p o. (p ~ (n + o), p ~ (m + o)) :- (n ~ m)
plusInj = Sub axiom
