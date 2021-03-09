{-# LANGUAGE ExistentialQuantification #-}

module Infrastructure.Sample where

import Data.Typeable (Typeable, cast)

data MockData = MockData
  { samples :: [Sample]
  , isInitiated :: Bool
  }

data Sample =
  forall a. Typeable a =>
            Sample a

makeSamples :: Typeable a => [a] -> [Sample]
makeSamples = map Sample

getSample :: Typeable b => Sample -> Maybe b
getSample (Sample x) = cast x
