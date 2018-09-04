-- |

module Test.Support.TypeEquality
  ( NominalEquality(..)
  , PropositionalEquality(..)
  , UnifiedEquality(..)
  ) where

import Mlem.Prelude
import Mlem.AST

newtype NominalEquality = NominalEquality Typ
  deriving newtype (Show)

instance Eq NominalEquality where
  (NominalEquality (VarT n)) == (NominalEquality (VarT m))
    = n == m
  (NominalEquality (AppT t1 u1)) == (NominalEquality (AppT t2 u2))
    = NominalEquality t1 == NominalEquality t2
    && NominalEquality u1 == NominalEquality u2
  (NominalEquality (ConT n1)) == (NominalEquality (ConT n2))
    = n1 == n2
  (NominalEquality ArrT) == (NominalEquality ArrT)
    = True
  (NominalEquality (TupleT n1)) == (NominalEquality (TupleT n2))
    = n1 == n2
  _ == _ = False

newtype PropositionalEquality = PropositionalEquality Typ

newtype UnifiedEquality       = UnifiedEquality Typ
