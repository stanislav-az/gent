{-# LANGUAGE BangPatterns #-}

module Infrastructure.Recording where

import Control.Monad.State (MonadState(get, state), modify')
import Data.List (uncons)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Ext.Control.Monad (inaction)
import Ext.Data.Either
import qualified Infrastructure.Action as An
import qualified Infrastructure.Sample as Sample
import qualified Infrastructure.TestT as Test

-- For testState
addAction :: T.Text -> [An.TestableItem] -> Test.TestT ()
addAction actionName actionArgs = modify' add
  where
    action = An.Action {..}
    add An.TestState {..} = An.TestState (Mid action : testState) mockData

initCallback :: T.Text -> [An.TestableItem] -> Test.TestT ()
initCallback actionName actionArgs = modify' add
  where
    callback = An.Callback An.Action {..}
    add An.TestState {..} = An.TestState (Lft callback : testState) mockData

yeildCallback :: Test.TestT ()
yeildCallback = modify' add
  where
    add An.TestState {..} =
      An.TestState (Rgt An.CallbackYeild : testState) mockData

getResult :: (a, An.TestState) -> a
getResult = fst

getActions :: (a, An.TestState) -> [Either An.Callback An.Action]
getActions = An.volumeTestState . snd

getTimedActions :: (a, An.TestState) -> [Either An.Callback An.Action]
getTimedActions = map reverseCallback . reverse . getActions
  where
    reverseCallback ::
         Either An.Callback An.Action -> Either An.Callback An.Action
    reverseCallback (Right action) = Right action
    reverseCallback (Left An.Callback {..}) =
      Left $ An.Callback callbackDescription (reverse callbackActions)

-- For mockData
initMockDataFor :: Typeable a => T.Text -> [a] -> Test.TestT ()
initMockDataFor actionName samples = do
  An.TestState {..} <- get
  maybe (modify' initiate) inaction $ Map.lookup actionName mockData
  where
    initiate An.TestState {..} =
      An.TestState testState $
      Map.insert actionName (Sample.makeSamples samples) mockData

-- TODO use throw instead of error ?
returnFor :: (Typeable a) => T.Text -> Test.TestT a
returnFor actionName =
  fromMaybe throwSampleTypeError . Sample.getSample <$> state takeout
  where
    takeout An.TestState {..} =
      let !thisActionData =
            fromMaybe throwNotFound $ Map.lookup actionName mockData
          !(!x, !rest) = fromMaybe throwNotSufficient $ uncons thisActionData
       in (x, An.TestState testState $ Map.insert actionName rest mockData)
    throwNotFound = error $ "Mock data for " <> show actionName <> " not found."
    throwNotSufficient =
      error $ "Not sufficient mock data for " <> show actionName
    throwSampleTypeError =
      error $ "Could not cast sample for " <> show actionName
