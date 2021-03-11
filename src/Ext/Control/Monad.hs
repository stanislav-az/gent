module Ext.Control.Monad
  ( inaction
  ) where

inaction :: (Applicative f) => a -> f ()
inaction = const $ pure ()
