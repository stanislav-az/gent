{-# LANGUAGE ExistentialQuantification #-}

module Infrastructure.Sample where

data Sample =
  forall a. Sample a
