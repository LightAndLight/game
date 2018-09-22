module Unique where

newtype Unique = Unique { unUnique :: Int }
  deriving (Eq, Ord, Show)

unsafeMkUnique :: Int -> Unique
unsafeMkUnique = Unique
