{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module ServiceHandle where

import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import qualified Core.Domain as Test
import qualified Core.Recorder.Recording as Test
import qualified Core.Recorder.Recorder as Test

-- % stack repl --no-load
-- λ> :l test/ServiceHandle.hs
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

handler :: Monad m => ServiceHandle m -> m T.Text
handler ServiceHandle {..} = do
  name <- whatsMyName 3.5 'u'
  setCurrentCounter 6
  fork subHandler
  withDependency 9 (const 'i')
  withCallback 77 handlerCallback
  pure name
  where
    subHandler = do
      setCurrentCounter 6
      setCurrentCounter 1
    handlerCallback _ = do
      setCurrentCounter 66
      name <- whatsMyName 38.5 'u'
      setCurrentCounter 7
      pure 'p'

-- TODO add this test to suite
testHandler :: IO ()
testHandler =
  let r = Test.runTest $ handler mockServiceHandle
   in do print $ Test.getResult r == "Mr. White"
         print $
           Test.getTimedActions r ==
           [ Test.packAction "whatsMyName" [Test.ti @Double 3.5, Test.ti 'u']
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
               , Test.Action "setCurrentCounter" [Test.ti @Integer 7]
               ]
           ]
