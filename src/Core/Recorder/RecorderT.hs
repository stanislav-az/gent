module Core.Recorder.RecorderT where

-- TODO use Control.Monad.Trans.Free.Church to move all needed instances to
-- underlying monad, not test transformer: RecorderT m a would have only Functor, Applicative, Monad
-- the most used case would be Test a = RecorderT Catch a
-- нужно делать все инстансы mtl: HasInstance m => instance HasInstance (RecorderT m)
