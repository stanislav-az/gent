module Core.Domain.Action
  ( Action(..)
  , Callback(..)
  , CallbackYeild(..)
  , makeCallback
  ) where

import Core.Domain.Testable (TestableItem)
import qualified Data.Text as T

data Action = Action
  { actionName :: T.Text
  , actionArgs :: [TestableItem]
  } deriving (Show, Eq)

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
