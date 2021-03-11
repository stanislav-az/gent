{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}

module Ext.Data.Either
  ( SumOfThree(..)
  ) where

import Data.Data (Data, Typeable)
import GHC.Generics (Generic)

data SumOfThree l m r
  = Lft l
  | Mid m
  | Rgt r
  deriving ( Eq
           , Ord
           , Show
           , Read
           , Functor
           , Foldable
           , Traversable
           , Generic
           , Typeable
           , Data
           )
