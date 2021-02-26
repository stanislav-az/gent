{-# LANGUAGE ScopedTypeVariables #-}

module Generics.ServiceHandle where

import qualified Data.Text as T
import qualified Infrastructure.Action as Test
import qualified Infrastructure.Recording as Test
import qualified Infrastructure.TestT as Test


generateMockImplementation :: forall handle. handle Test.TestT
generateMockImplementation = undefined

data ServiceHandle m = ServiceHandle
  { whatsMyName       :: Double -> Char -> m T.Text
  , setCurrentCounter :: Integer -> m ()
  , fork :: m () -> m ()
  , withCallback :: Int -> m Char -> m ()
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
          Test.addAction action
          pure "Mr. White"
    , setCurrentCounter =
        \i -> do
          let action = Test.Action "setCurrentCounter" [Test.TestableItem i]
          Test.addAction action
          pure ()
    , fork = \ma -> do
          let action = Test.Action "fork" [Test.TestableItem i]
          pure ()
    , withCallback = undefined
    }
