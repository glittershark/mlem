{-# OPTIONS_GHC -fno-warn-orphans #-}
--------------------------------------------------------------------------------
-- |
-- Module      : Mlem.Orphans.Arbitrary
-- Description : Arbitrary instances
--
--
--------------------------------------------------------------------------------
module Mlem.Orphans.Arbitrary
  (
  ) where
--------------------------------------------------------------------------------
import Mlem.Prelude
import Data.List.NonEmpty (NonEmpty(..))
--------------------------------------------------------------------------------
import Test.QuickCheck.Arbitrary
--------------------------------------------------------------------------------

instance Arbitrary Text where
  arbitrary = pack <$> arbitrary
  shrink    = map pack . shrink . unpack

instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = (:|) <$> arbitrary <*> arbitrary
