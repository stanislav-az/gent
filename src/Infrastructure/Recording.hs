module Infrastructure.Recording where

import Control.Monad.State (modify)
import qualified Data.Text as T
import qualified Infrastructure.Action as An
import qualified Infrastructure.TestT as Test

addAction :: T.Text -> [An.TestableItem] -> Test.TestT ()
addAction actionName actionArgs = modify add
  where
    add An.TestState {..} =
      An.TestState (An.packAction actionName actionArgs : testState) mockData

addCallback :: T.Text -> [An.TestableItem] -> [An.Action] -> Test.TestT ()
addCallback actionName actionArgs callbackActions = modify add
  where
    add An.TestState {..} =
      An.TestState
        (An.packCallback actionName actionArgs callbackActions : testState)
        mockData

getResult :: (a, An.TestState) -> a
getResult = fst

getActions :: (a, An.TestState) -> [Either An.Callback An.Action]
getActions = An.testState . snd

getTimedActions :: (a, An.TestState) -> [Either An.Callback An.Action]
getTimedActions = reverse . getActions

getOnlyActions :: (a, An.TestState) -> [An.Action]
getOnlyActions = An.flattenTestState . snd
