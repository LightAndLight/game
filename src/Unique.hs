module Unique
  ( Unique, unUnique, unsafeMkUnique, Supply, HasSupply(..)
  , newSupply, getUnique
  )
where

import Control.Concurrent.Supply (Supply, newSupply, freshId)
import Control.Lens.Getter (uses)
import Control.Lens.Lens (Lens')
import Control.Lens.Setter (assign)
import Control.Monad.State (MonadState)

newtype Unique = Unique { unUnique :: Int }
  deriving (Eq, Ord, Show)

class HasSupply s where
  supply :: Lens' s Supply

getUnique :: (HasSupply s, MonadState s m) => m Unique
getUnique = do
  (id, s') <- uses supply freshId
  assign supply s'
  pure $ Unique id

unsafeMkUnique :: Int -> Unique
unsafeMkUnique = Unique
