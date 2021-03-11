module Core.Domain.TestState
  ( TestState(..)
  , emptyTestState
  , packAction
  , packCallback
  , volumeTestState
  ) where

import Core.Domain.Action
  ( Action(..)
  , Callback(..)
  , CallbackYeild(..)
  , makeCallback
  )
import Core.Domain.Sample (Sample)
import Core.Domain.Testable (TestableItem)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Ext.Data.Either (SumOfThree(..))

data TestState = TestState
  { testState :: [SumOfThree ([Action] -> Callback) Action CallbackYeild]
  , mockData :: Map.Map T.Text [Sample]
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
