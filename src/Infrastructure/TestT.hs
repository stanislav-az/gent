{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Infrastructure.TestT where

import           Control.Applicative (Alternative)
import           Control.Monad.Catch.Pure (Catch, CatchT (runCatchT),
                                           MonadCatch, MonadMask, MonadThrow,
                                           SomeException)
import           Control.Monad.Identity (Identity (runIdentity))
import           Control.Monad.State (MonadPlus, MonadState, StateT (..))
import qualified Infrastructure.Action as An

-- TODO use Control.Monad.Trans.Free.Church to move all needed instances to
-- underlying monad, not test transformer: TestT m a would have only Functor, Applicative, Monad
-- the most used case would be Test a = TestT Catch a
-- нужно делать все инстансы mtl: HasInstance m => instance HasInstance (TestT m)
newtype TestT a = TestT
  { runTestT :: StateT [An.Action] Catch a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadState [An.Action]
             , MonadThrow
             , MonadCatch
             , MonadMask
             , MonadFail
             , Alternative
             , MonadPlus
             )

runTestSafe :: TestT a -> Either SomeException (a, [An.Action])
runTestSafe = runIdentity . runCatchT . (`runStateT` []) . runTestT

runTest :: TestT a -> (a, [An.Action])
runTest test =
  either (error . ("Uncaught exception during runTest: " <>) . show) id $
  runTestSafe test
