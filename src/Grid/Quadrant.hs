module Grid.Quadrant where

newtype Quadrant = Quadrant (Int, Int)
  deriving (Eq, Show, Ord)
