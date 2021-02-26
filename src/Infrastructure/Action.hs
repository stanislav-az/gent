{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeApplications          #-}

module Infrastructure.Action where

import qualified Data.Text as T
import qualified Data.Typeable as Type

data Action = Action
  { actionName :: T.Text
  , actionArgs :: [TestableItem]
  } deriving (Show,Eq)

type Testable a = (Type.Typeable a, Eq a, Show a)

data TestableItem =
  forall a. Testable a =>
            TestableItem a

deriving instance Show TestableItem

instance Eq TestableItem where
  (TestableItem (x :: a)) == (TestableItem (y :: b))
    | Just Type.Refl <- Type.eqT @a @b = x == y
    | otherwise = False
