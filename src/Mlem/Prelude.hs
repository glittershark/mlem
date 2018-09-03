-- |

module Mlem.Prelude
  ( module ClassyPrelude
  , Type
  , Constraint
  , module GHC.TypeLits
  ) where

import ClassyPrelude hiding (return)
import Data.Kind hiding ((*))
import GHC.TypeLits hiding (Text)
