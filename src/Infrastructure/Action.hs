{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Infrastructure.Action where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Typeable as Type
import Ext.Data.Either
import qualified Infrastructure.Sample as Sample

data TestState = TestState
  { testState :: [SumOfThree ([Action] -> Callback) Action CallbackYeild]
  , mockData :: Map.Map T.Text [Sample.Sample]
  }

emptyTestState :: TestState
emptyTestState = TestState [] Map.empty

packAction :: T.Text -> [TestableItem] -> Either Callback Action
packAction actionName actionArgs = Right Action {..}

packCallback :: T.Text -> [TestableItem] -> [Action] -> Either Callback Action
packCallback actionName actionArgs callbackActions =
  Left $ makeCallback actionName actionArgs callbackActions

volumeTestState :: TestState -> [Either Callback Action]
volumeTestState TestState {..} = snd $ foldr getActions (Nothing, []) testState
  where
    getActions ::
         SumOfThree ([Action] -> Callback) Action CallbackYeild
      -> (Maybe Callback, [Either Callback Action])
      -> (Maybe Callback, [Either Callback Action])
    getActions (Lft mkCallback) (_, previous) = (Just $ mkCallback [], previous)
    getActions (Rgt CallbackYeild) (Just callback, previous) =
      (Nothing, Left callback : previous)
    getActions (Mid action) (Just callback, previous) =
      (Just $ insertAction action callback, previous)
    getActions (Mid action) (Nothing, previous) =
      (Nothing, Right action : previous)
    insertAction :: Action -> Callback -> Callback
    insertAction an Callback {..} =
      Callback callbackDescription (an : callbackActions)

data Callback = Callback
  { callbackDescription :: Action
  , callbackActions :: [Action]
  } deriving (Show, Eq)

data CallbackYeild =
  CallbackYeild
  deriving (Show, Eq)

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
