{-# language RoleAnnotations #-}
module Dimensions where

newtype Width a = Width { unWidth :: a }
  deriving (Eq, Show)
type role Width nominal

newtype Height a = Height { unHeight :: a }
  deriving (Eq, Show)
type role Height nominal
