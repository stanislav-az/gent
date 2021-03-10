{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Generics.ServiceHandle where

import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import qualified Infrastructure.Action as Test
import qualified Infrastructure.Recording as Test
import qualified Infrastructure.TestT as Test

generateMockImplementation :: forall handle. handle Test.TestT
generateMockImplementation = undefined

data ServiceHandle m = ServiceHandle
  { whatsMyName :: Double -> Char -> m T.Text
  , setCurrentCounter :: Integer -> m ()
  , fork :: m () -> m ()
  , function :: String -> Maybe Char
  , constant :: T.Text
  , withDependency :: Int -> (T.Text -> Char) -> m ()
  , withCallback :: Int -> (T.Text -> m Char) -> m ()
  }

mockServiceHandle :: ServiceHandle Test.TestT
mockServiceHandle =
  ServiceHandle
    { whatsMyName =
        \d c -> do
          Test.addAction
            "whatsMyName"
            [Test.TestableItem d, Test.TestableItem c]
          Test.initMockDataFor "whatsMyName" ["Mr. White" :: T.Text, "White"]
          Test.returnFor "whatsMyName"
    , setCurrentCounter =
        \i -> do
          Test.addAction "setCurrentCounter" [Test.TestableItem i]
          Test.initMockDataFor "setCurrentCounter" $ replicate 5 ()
          Test.returnFor "setCurrentCounter"
    , fork =
        \ma -> do
          Test.initCallback "fork" []
          ma
          Test.yeildCallback
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
          Test.initCallback "withCallback" [Test.TestableItem i]
          ma
          Test.yeildCallback
          pure ()
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
