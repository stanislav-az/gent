{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Infrastructure.Action where

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Typeable as Type
import qualified Infrastructure.Sample as Sample

data TestState = TestState
  { testState :: [Either Callback Action]
  , mockData :: Map.Map T.Text [Sample.Sample]
  }

emptyTestState :: TestState
emptyTestState = TestState [] Map.empty

packAction :: T.Text -> [TestableItem] -> Either Callback Action
packAction actionName actionArgs = Right Action {..}

packCallback :: T.Text -> [TestableItem] -> [Action] -> Either Callback Action
packCallback actionName actionArgs callbackActions = Left $ makeCallback actionName actionArgs callbackActions

flattenTestState :: TestState -> [Action]
flattenTestState TestState {..} = foldr getActions [] testState
  where
    getActions :: Either Callback Action -> [Action] -> [Action]
    getActions (Left Callback {..}) previous = callbackDescription : callbackActions <> previous
    getActions (Right action) previous = action : previous

data Callback = Callback
  { callbackDescription :: Action
  , callbackActions :: [Action]
  } deriving (Show, Eq)

makeCallback :: T.Text -> [TestableItem] -> [Action] -> Callback
makeCallback actionName actionArgs callbackActions = Callback {..}
  where
    callbackDescription = Action {..}

data Action = Action
  { actionName :: T.Text
  , actionArgs :: [TestableItem]
  } deriving (Show, Eq)

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
