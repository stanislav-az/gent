{-# LANGUAGE ScopedTypeVariables #-}

module Generics.ServiceHandle where

import           Data.Maybe               (listToMaybe)
import qualified Data.Text                as T
import qualified Infrastructure.Action    as Test
import qualified Infrastructure.Recording as Test
import qualified Infrastructure.TestT     as Test

generateMockImplementation :: forall handle. handle Test.TestT
generateMockImplementation = undefined

data ServiceHandle m = ServiceHandle
  { whatsMyName       :: Double -> Char -> m T.Text
  , setCurrentCounter :: Integer -> m ()
  , fork              :: m () -> m ()
  , function          :: String -> Maybe Char
  , constant          :: T.Text
  , withDependency    :: Int -> (T.Text -> Char) -> m ()
  , withCallback      :: Int -> (T.Text -> m Char) -> m ()
  }

mockServiceHandle :: ServiceHandle Test.TestT
mockServiceHandle =
  ServiceHandle
    { whatsMyName =
        \d c -> do
          let action =
                Test.Action
                  "whatsMyName"
                  [Test.TestableItem d, Test.TestableItem c]
                  Nothing
          Test.addAction action
          pure "Mr. White"
    , setCurrentCounter =
        \i -> do
          let action = Test.Action "setCurrentCounter" [Test.TestableItem i] Nothing
          Test.addAction action
          pure ()
    , fork =
        \ma -> do
          let (_, forkedActions) = Test.runTest ma
          let action =
                Test.Action "fork" []
                (Just forkedActions)
          Test.addAction action
          pure ()
    , function = listToMaybe
    , constant = "asdf"
    , withDependency =
        \i _f -> do
          let action = Test.Action "withDependency" [Test.TestableItem i] Nothing
          Test.addAction action
          pure ()
    , withCallback =
        \i cb -> do
          let ma = cb "mock"
          let (res, callbackActions) = Test.runTest ma
          let action = Test.Action "withCallback" [Test.TestableItem i] (Just callbackActions)
          Test.addAction action
          pure ()
    }
