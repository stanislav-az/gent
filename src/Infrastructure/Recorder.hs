{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Infrastructure.Recorder where

import           Control.Applicative (Alternative)
import           Control.Monad.Catch.Pure (Catch, CatchT (runCatchT),
                                           MonadCatch, MonadMask, MonadThrow,
                                           SomeException)
import           Control.Monad.Identity (Identity (runIdentity))
import           Control.Monad.State (MonadPlus, MonadState, StateT (..))
import qualified Infrastructure.Action as An

-- TODO rename to recorder?
-- TODO use Control.Monad.Trans.Free.Church to move all needed instances to
-- underlying monad, not test transformer: Recorder m a would have only Functor, Applicative, Monad
-- the most used case would be Test a = Recorder Catch a
-- нужно делать все инстансы mtl: HasInstance m => instance HasInstance (Recorder m)
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
runTestSafe = runIdentity . runCatchT . (`runStateT` An.emptyTestState) . runRecorder

runTest :: Recorder a -> (a, An.TestState)
runTest test =
  either (error . ("Uncaught exception during runTest: " <>) . show) id $
  runTestSafe test
