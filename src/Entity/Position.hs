{-# language FunctionalDependencies, MultiParamTypeClasses #-}
{-# language RecordWildCards #-}
module Entity.Position where

import Reflex
import Control.Lens.Getter ((^.))
import Control.Lens.Lens (Lens')
import Control.Monad.Fix (MonadFix)
import Linear.V2 (V2(..), _x, _y)

import Dimensions (Width(..), Height(..))
import Map (Map(..))

class HasPosition t e | e -> t where
  position :: Lens' e (Dynamic t (V2 Float))

mkEntityPosition
  :: (Reflex t, MonadHold t m, MonadFix m)
  => Map
  -> Width Float
  -> Height Float
  -> V2 Float -- ^ initial position
  -> Event t Float -- ^ when x coordinate changed
  -> Event t Float -- ^ when y coordinate changed
  -> m (Dynamic t (V2 Float))
mkEntityPosition Map{..} w h pos eX eY = do
  dX <-
    holdUniqDyn =<<
    holdDyn
      (pos^._x)
      (max 0 . min (unWidth _mapWidth - unWidth w) <$> eX)

  dY <-
    holdUniqDyn =<<
    holdDyn
      (pos^._y)
      (max 0 . min (unHeight _mapHeight - unHeight h) <$> eY)

  pure $ V2 <$> dX <*> dY
