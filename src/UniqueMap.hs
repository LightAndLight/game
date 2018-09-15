{-# language GeneralizedNewtypeDeriving #-}
module UniqueMap where

import Reflex
import Control.Lens.Setter (over, mapped)
import Control.Lens.Tuple (_1)
import Control.Monad.Fix (MonadFix)
import Data.Coerce (coerce)

import qualified Data.IntMap.Strict as IntMap

import Unique (Unique, unsafeMkUnique, unUnique)

newtype UniqueMap a = UniqueMap { unUniqueMap :: IntMap.IntMap a }
  deriving (Functor, Foldable)

insert :: Unique -> a -> UniqueMap a -> UniqueMap a
insert u a = coerce $ IntMap.insert (unUnique u) a

empty :: UniqueMap a
empty = UniqueMap IntMap.empty

delete :: Unique -> UniqueMap a -> UniqueMap a
delete u m = UniqueMap $ IntMap.delete (unUnique u) (unUniqueMap m)

lookup :: Unique -> UniqueMap a -> Maybe a
lookup u m = IntMap.lookup (unUnique u) (unUniqueMap m)

fromList :: [(Unique, a)] -> UniqueMap a
fromList = UniqueMap . IntMap.fromList . over (mapped._1) unUnique

mergeWithKey
  :: (Unique -> a -> b -> Maybe c)
  -> (UniqueMap a -> UniqueMap c)
  -> (UniqueMap b -> UniqueMap c)
  -> UniqueMap a
  -> UniqueMap b
  -> UniqueMap c
mergeWithKey m f g =
  coerce (IntMap.mergeWithKey (m . unsafeMkUnique) (coerce f) (coerce g))

mapMaybe :: (a -> Maybe b) -> UniqueMap a -> UniqueMap b
mapMaybe f = coerce (IntMap.mapMaybe f)

uniqueMapHold
  :: (Reflex t, MonadHold t m, MonadFix m)
  => UniqueMap a
  -> Event t (UniqueMap (Maybe a))
  -> m (Dynamic t (UniqueMap a))
uniqueMapHold uMap eUpdate =
  foldDyn (mergeWithKey (\_ ma _ -> ma) (mapMaybe id) id) uMap eUpdate

mergeUnique :: Reflex t => UniqueMap (Event t a) -> Event t (UniqueMap a)
mergeUnique um = UniqueMap <$> mergeInt (unUniqueMap um)
