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
          Test.addAction "whatsMyName"
                  [Test.TestableItem d, Test.TestableItem c]
          pure "Mr. White"
    , setCurrentCounter =
        \i -> do
          Test.addAction "setCurrentCounter" [Test.TestableItem i]
          pure ()
    , fork =
        \ma -> do
          let forkedActions = Test.getOnlyActions $ Test.runTest ma
          Test.addCallback "fork" [] forkedActions
          pure ()
    , function = listToMaybe
    , constant = "asdf"
    , withDependency =
        \i _f -> do
          Test.addAction "withDependency" [Test.TestableItem i]
          pure ()
    , withCallback =
        \i cb -> do
          let ma = cb "mock"
          let callbackActions = Test.getOnlyActions $ Test.runTest ma
          Test.addCallback "withCallback" [Test.TestableItem i] callbackActions
          pure ()
    }
