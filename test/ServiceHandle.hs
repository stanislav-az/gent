{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module ServiceHandle
  ( handlerSpec
  ) where

import qualified Core.Domain as Test
import qualified Core.Recorder.Recorder as Test
import qualified Core.Recorder.Recording as Test
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import Test.Hspec (Spec, describe, it, shouldBe)

class Monad m =>
      MonadLogger m
  where
  logInfo :: T.Text -> m ()
  logError :: T.Text -> m ()

instance MonadLogger Test.Recorder where
  logInfo t = Test.mockActionAlwaysReturn "logInfo" [Test.ti t] ()
  logError t = Test.mockActionAlwaysReturn "logError" [Test.ti t] ()

data ServiceHandle m = ServiceHandle
  { whatsMyName :: Double -> Char -> m T.Text
  , setCurrentCounter :: Integer -> m ()
  , fork :: m () -> m ()
  , function :: String -> Maybe Char
  , constant :: T.Text
  , withDependency :: Int -> (T.Text -> Char) -> m ()
  , withCallback :: Int -> (T.Text -> m Char) -> m ()
  }

mockServiceHandle :: ServiceHandle Test.Recorder
mockServiceHandle =
  ServiceHandle
    { whatsMyName =
        \d c ->
          Test.mockAction
            "whatsMyName"
            [Test.TestableItem d, Test.TestableItem c]
            ["Mr. White" :: T.Text, "White"]
    , setCurrentCounter =
        \i ->
          Test.mockAction "setCurrentCounter" [Test.TestableItem i] $
          replicate 5 ()
    , fork = \ma -> Test.mockCallback "fork" ma [] [()]
    , function = listToMaybe -- Pure functions are not tested
    , constant = "asdf" -- Pure values are not tested
    , withDependency =
        \i _f -> Test.mockAction "withDependency" [Test.TestableItem i] [()]
    , withCallback =
        \i cb -> do
          let ma = cb "mock"
          Test.mockCallback "withCallback" ma [Test.TestableItem i] [()]
    }

handler :: MonadLogger m => ServiceHandle m -> m T.Text
handler ServiceHandle {..} = do
  logInfo "starting handler"
  name <- whatsMyName 3.5 'u'
  setCurrentCounter 6
  fork subHandler
  withDependency 9 (const 'i')
  withCallback 77 handlerCallback
  logInfo "successful execution of handler"
  pure name
  where
    subHandler = do
      setCurrentCounter 6
      setCurrentCounter 1
    handlerCallback _ = do
      setCurrentCounter 66
      name <- whatsMyName 38.5 'u'
      logError $ "Name was: " <> name
      setCurrentCounter 7
      pure 'p'

handlerSpec :: Spec
handlerSpec =
  describe "handler" $ do
    it "Should execute successfully" $ do
      let r = Test.runTest $ handler mockServiceHandle
      Test.getResult r `shouldBe` "Mr. White"
      Test.getTimedActions r `shouldBe`
        [ Test.packAction "logInfo" [Test.ti @T.Text "starting handler"]
        , Test.packAction "whatsMyName" [Test.ti @Double 3.5, Test.ti 'u']
        , Test.packAction "setCurrentCounter" [Test.ti @Integer 6]
        , Test.packCallback
            "fork"
            []
            [ Test.Action "setCurrentCounter" [Test.ti @Integer 6]
            , Test.Action "setCurrentCounter" [Test.ti @Integer 1]
            ]
        , Test.packAction "withDependency" [Test.ti @Int 9]
        , Test.packCallback
            "withCallback"
            [Test.ti @Int 77]
            [ Test.Action "setCurrentCounter" [Test.ti @Integer 66]
            , Test.Action "whatsMyName" [Test.ti @Double 38.5, Test.ti 'u']
            , Test.Action "logError" [Test.ti @T.Text "Name was: White"]
            , Test.Action "setCurrentCounter" [Test.ti @Integer 7]
            ]
        , Test.packAction
            "logInfo"
            [Test.ti @T.Text "successful execution of handler"]
        ]
