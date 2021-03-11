module Core.Recorder.RecorderT where

-- TODO implement (RecorderT m a) transformer (probably add mtl-style type class for it also)
-- move all needed instances to underlying monad, not test transformer:
-- (RecorderT m) would have only Functor, Applicative, Monad
-- add instances for all mtl type classes: HasInstance m => instance HasInstance (RecorderT m)
-- Implementation options:
-- 1. pack in all needed inside newtype: RecorderT m a = RecorderT (m (Recorder a))
-- 2. use Control.Monad.Trans.Free.Church
