{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}

module Core.Recorder.Exception
  ( RecorderException(..)
  ) where

import Control.Exception (Exception(displayException))
import qualified Data.Text as T

data RecorderException
  = MockDataNotFound { getActionName :: T.Text }
  | MockDataNotSufficient { getActionName :: T.Text }
  | SampleTypeError { getActionName :: T.Text }
  deriving stock Eq
  deriving anyclass Exception

instance Show RecorderException where
  show e =
    "Exception with action " <> T.unpack (getActionName e) <> " : " <>
    case e of
      MockDataNotFound _ -> "Mock data not found."
      MockDataNotSufficient _ -> "Mock data not sufficient."
      SampleTypeError _ -> "Sample type error."
