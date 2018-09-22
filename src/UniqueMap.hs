{-# language GeneralizedNewtypeDeriving #-}
module UniqueMap where

import Reflex
import Control.Lens.Setter (over, mapped)
import Control.Lens.Tuple (_1)
import Control.Monad.Fix (MonadFix)
import Data.Coerce (coerce)
import Data.Semigroup (Semigroup)

import qualified Data.IntMap.Strict as IntMap

import Unique (Unique, unsafeMkUnique, unUnique)

newtype UniqueMap a = UniqueMap { unUniqueMap :: IntMap.IntMap a }
  deriving (Functor, Foldable, Semigroup, Show, Monoid)

insert :: Unique -> a -> UniqueMap a -> UniqueMap a
insert u a = coerce $ IntMap.insert (unUnique u) a

member :: Unique -> UniqueMap a -> Bool
member u um = IntMap.member (unUnique u) (unUniqueMap um)

singleton :: Unique -> a -> UniqueMap a
singleton u a = UniqueMap $ IntMap.singleton (unUnique u) a

empty :: UniqueMap a
empty = UniqueMap IntMap.empty

delete :: Unique -> UniqueMap a -> UniqueMap a
delete u m = UniqueMap $ IntMap.delete (unUnique u) (unUniqueMap m)

lookup :: Unique -> UniqueMap a -> Maybe a
lookup u m = IntMap.lookup (unUnique u) (unUniqueMap m)

foldrWithKey :: (Unique -> a -> b -> b) -> b -> UniqueMap a -> b
foldrWithKey a b c =
  IntMap.foldrWithKey (a . unsafeMkUnique) b (unUniqueMap c)

alter :: (Maybe a -> Maybe a) -> Unique -> UniqueMap a -> UniqueMap a
alter f u = coerce $ IntMap.alter f (unUnique u)

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

filter :: (a -> Bool) -> UniqueMap a -> UniqueMap a
filter f = coerce (IntMap.filter f)

uniqueMapHold
  :: (Reflex t, MonadHold t m, MonadFix m)
  => UniqueMap a
  -> Event t (UniqueMap (Maybe a))
  -> m (Dynamic t (UniqueMap a))
uniqueMapHold uMap eUpdate =
  foldDyn (mergeWithKey (\_ ma _ -> ma) (mapMaybe id) id) uMap eUpdate

mergeUnique :: Reflex t => UniqueMap (Event t a) -> Event t (UniqueMap a)
mergeUnique um = UniqueMap <$> mergeInt (unUniqueMap um)
