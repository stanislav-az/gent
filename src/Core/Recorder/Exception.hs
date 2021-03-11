module Core.Recorder.Exception
  ( RecorderException(..)
  ) where

import Control.Exception (Exception(displayException))
import qualified Data.Text as T

data RecorderException
  = MockDataNotFound { getActionName :: T.Text }
  | MockDataNotSufficient { getActionName :: T.Text }
  | SampleTypeError { getActionName :: T.Text }
  deriving (Show, Eq)

instance Exception RecorderException where
  displayException e =
    "Exception with action " <> T.unpack (getActionName e) <> " : " <>
    case e of
      MockDataNotFound _ -> "Mock data not found."
      MockDataNotSufficient _ -> "Mock data not sufficient."
      SampleTypeError _ -> "Sample type error."
