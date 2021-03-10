module Ext.Control.Monad where

inaction :: (Applicative f) => a -> f ()
inaction = const $ pure ()
