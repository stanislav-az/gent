{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}

module Core.Domain.Testable
  ( Testable
  , TestableItem(..)
  , ti
  ) where

import qualified Data.Typeable as Type

type Testable a = (Type.Typeable a, Eq a, Show a)

data TestableItem =
  forall a. Testable a =>
            TestableItem a

deriving instance Show TestableItem

ti :: Testable a => a -> TestableItem
ti = TestableItem

instance Eq TestableItem where
  (TestableItem (x :: a)) == (TestableItem (y :: b))
    | Just Type.Refl <- Type.eqT @a @b = x == y
    | otherwise = False
