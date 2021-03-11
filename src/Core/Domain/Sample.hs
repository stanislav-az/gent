{-# LANGUAGE ExistentialQuantification #-}

module Core.Domain.Sample where

import Data.Typeable (Typeable, cast)

data Sample =
  forall a. Typeable a =>
            Sample a

makeSamples :: Typeable a => [a] -> [Sample]
makeSamples = map Sample

getSample :: Typeable b => Sample -> Maybe b
getSample (Sample x) = cast x
