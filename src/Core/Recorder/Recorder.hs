{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Core.Recorder.Recorder
  ( Recorder(..)
  , runTest
  , runTestSafe
  ) where

import Control.Applicative (Alternative)
import Control.Monad.Catch.Pure
  ( Catch
  , CatchT(runCatchT)
  , MonadCatch
  , MonadMask
  , MonadThrow
  , SomeException
  )
import Control.Monad.Identity (Identity(runIdentity))
import Control.Monad.State (MonadPlus, MonadState, StateT(..))
import qualified Core.Domain as An

newtype Recorder a = Recorder
  { runRecorder :: StateT An.TestState Catch a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadState An.TestState
             , MonadThrow
             , MonadCatch
             , MonadMask
             , MonadFail
             , Alternative
             , MonadPlus
             )

runTestSafe :: Recorder a -> Either SomeException (a, An.TestState)
runTestSafe =
  runIdentity . runCatchT . (`runStateT` An.emptyTestState) . runRecorder

runTest :: Recorder a -> (a, An.TestState)
runTest test =
  either (error . ("Uncaught exception during runTest: " <>) . show) id $
  runTestSafe test
