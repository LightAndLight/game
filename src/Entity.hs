{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies, MultiParamTypeClasses #-}
{-# language DefaultSignatures #-}
{-# language RecordWildCards #-}
{-# language RecursiveDo #-}
{-# language TemplateHaskell #-}
module Entity where

import Reflex
import Control.Lens.Getter ((^.))
import Control.Lens.Lens (Lens')
import Control.Lens.TH (makeLenses)
import Control.Monad.Fix (MonadFix)
import Graphics.Gloss (Picture)
import Linear.V2 (V2(..), R1(..), R2(..))

import Dimensions (Width(..), Height(..), HasWidth(..), HasHeight(..))
import Grid.Quadrant (Quadrant)
import GridManager.Class (GridManager, registerEntity, getQuadrants)
import Map (Map(..))
import Position (HasPosition(..))
import Unique (Unique)

data Entity t
  = Entity
  { _entityQuadrants :: Dynamic t [Quadrant]
  , _entityPosition :: Dynamic t (V2 Float)
  , _entityPicture :: Dynamic t Picture
  , _entityWidth :: Width Float
  , _entityHeight :: Height Float
  }
makeLenses ''Entity

class HasQuadrants t e | e -> t where
  quadrants :: Lens' e (Dynamic t [Quadrant])

class HasPicture t e | e -> t where
  picture :: Lens' e (Dynamic t Picture)

class ToEntity t e | e -> t where
  toEntity :: e -> Entity t
  default toEntity
    :: ( HasQuadrants t e, HasPosition t e, HasPicture t e
       , HasWidth e, HasHeight e
       )
    => e -> Entity t
  toEntity e =
    Entity
    { _entityQuadrants = e ^. quadrants
    , _entityPosition = e ^. position
    , _entityPicture = e ^. picture
    , _entityWidth = e ^. width
    , _entityHeight = e ^. height
    }

instance HasPicture t (Entity t) where; picture = entityPicture
instance HasPosition t (Entity t) where; position = entityPosition
instance HasQuadrants t (Entity t) where; quadrants = entityQuadrants
instance HasWidth (Entity t) where; width = entityWidth
instance HasHeight (Entity t) where; height = entityHeight

mkEntityPos
  :: (Reflex t, MonadHold t m, MonadFix m)
  => Map
  -> Width Float
  -> Height Float
  -> V2 Float -- ^ initial position
  -> Event t Float -- ^ when x coordinate changed
  -> Event t Float -- ^ when y coordinate changed
  -> m (Dynamic t (V2 Float))
mkEntityPos Map{..} w h pos eX eY = do
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

mkEntity
  :: ( MonadHold t m, MonadFix m
     , ToEntity t e
     , GridManager t (Entity t) m
     )
  => Unique
  -> e
  -> m (Dynamic t [Quadrant])
mkEntity u e = do
  registerEntity u $ toEntity e
  getQuadrants u

intersects
  :: Reflex t
  => Entity t
  -> Entity t
  -> Dynamic t Bool
intersects e1 e2 =
  (\e1Qs e2Qs e1Left e1Top e2Left e2Top ->
    let
      e1Right = e1Left + unWidth (e1^.entityWidth)
      e1Bottom = e1Top + unHeight (e1^.entityHeight)
      e2Right = e2Left + unWidth (e2^.entityWidth)
      e2Bottom = e2Top + unHeight (e2^.entityHeight)
    in
      any (`elem` e1Qs) e2Qs &&
      not
        (e1Right < e2Left ||
         e1Top > e2Bottom ||
         e1Left > e2Right ||
         e1Bottom < e2Top)) <$>
  (e1^.entityQuadrants) <*>
  (e2^.entityQuadrants) <*>
  ((^. _x) <$> e1^.entityPosition) <*>
  ((^. _y) <$> e1^.entityPosition) <*>
  ((^. _x) <$> e2^.entityPosition) <*>
  ((^. _y) <$> e2^.entityPosition)
