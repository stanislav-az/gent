{-# LANGUAGE BangPatterns #-}

module Core.Recorder.Recording
  ( addAction
  , initCallback
  , yeildCallback
  , getResult
  , getActions
  , getTimedActions
  , initMockDataFor
  , returnFor
  , mockAction
  , mockCallback
  , mockActionAlwaysReturn
  , mockCallbackAlwaysReturn
  ) where

import Control.Monad.Catch (MonadThrow(throwM))
import Control.Monad.State (MonadState(get, put, state), modify')
import qualified Core.Domain as An
import Core.Recorder.Exception (RecorderException(..))
import qualified Core.Recorder.Recorder as Test
import Data.List (uncons)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Ext.Control.Monad (inaction)
import Ext.Data.Either (SumOfThree(Lft, Mid, Rgt))

-- For testState
addAction :: T.Text -> [An.TestableItem] -> Test.Recorder ()
addAction actionName actionArgs = modify' add
  where
    action = An.Action {..}
    add An.TestState {..} = An.TestState (Mid action : testState) mockData

initCallback :: T.Text -> [An.TestableItem] -> Test.Recorder ()
initCallback actionName actionArgs = modify' add
  where
    callback = An.Callback An.Action {..}
    add An.TestState {..} = An.TestState (Lft callback : testState) mockData

yeildCallback :: Test.Recorder ()
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
initMockDataFor :: Typeable a => T.Text -> [a] -> Test.Recorder ()
initMockDataFor actionName samples = do
  An.TestState {..} <- get
  maybe (modify' initiate) inaction $ Map.lookup actionName mockData
  where
    initiate An.TestState {..} =
      An.TestState testState $
      Map.insert actionName (An.makeSamples samples) mockData

returnFor :: (Typeable a) => T.Text -> Test.Recorder a
returnFor actionName = do
  An.TestState {..} <- get
  !thisActionData <- maybe throwNotFound pure $ Map.lookup actionName mockData
  (!x, !rest) <- maybe throwNotSufficient pure $ uncons thisActionData
  put $ An.TestState testState $ Map.insert actionName rest mockData
  maybe throwSampleTypeError pure $ An.getSample x
  where
    throwNotFound :: Test.Recorder r
    throwNotFound = throwM $ MockDataNotFound actionName
    throwNotSufficient :: Test.Recorder r
    throwNotSufficient = throwM $ MockDataNotSufficient actionName
    throwSampleTypeError :: Test.Recorder r
    throwSampleTypeError = throwM $ SampleTypeError actionName

-- For using in mocks
mockAction ::
     (Typeable a, Typeable b)
  => T.Text
  -> [An.TestableItem]
  -> [a]
  -> Test.Recorder b
mockAction actionName args samples = do
  addAction actionName args
  initMockDataFor actionName samples
  returnFor actionName

mockActionAlwaysReturn :: T.Text -> [An.TestableItem] -> a -> Test.Recorder a
mockActionAlwaysReturn actionName args sample = do
  addAction actionName args
  pure sample

mockCallback ::
     (Typeable a, Typeable b)
  => T.Text
  -> Test.Recorder r
  -> [An.TestableItem]
  -> [a]
  -> Test.Recorder b
mockCallback callbackName callback args samples = do
  initCallback callbackName args
  callback
  yeildCallback
  initMockDataFor callbackName samples
  returnFor callbackName

mockCallbackAlwaysReturn ::
     T.Text -> Test.Recorder r -> [An.TestableItem] -> a -> Test.Recorder a
mockCallbackAlwaysReturn callbackName callback args sample = do
  initCallback callbackName args
  callback
  yeildCallback
  pure sample
