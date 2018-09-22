{-# language FlexibleContexts #-}
{-# language RecordWildCards #-}
{-# language RecursiveDo #-}
{-# language TemplateHaskell #-}
module Entity
  ( Entity
  , mkEntityPos
  , mkStaticEntity
  , mkMovingEntity
  , entityId
  , entityPicture
  , entityWidth
  , entityHeight
  , entityPosition
  , entityQuadrants
  , intersects
  , HasEntity(..)
  )
where

import Reflex

import Control.Lens.Getter ((^.), Getter)
import Control.Lens.Lens (Lens')
import Control.Lens.TH (makeLensesFor)
import Control.Monad.Fix (MonadFix)
import Graphics.Gloss (Picture)
import Linear.V2 (V2(..), R1(..), R2(..))

import Dimensions (Width(..), Height(..))
import Grid (Quadrant)
import GridManager.Class (GridManager, registerEntity, getQuadrants)
import Map (Map(..))
import Unique (Unique)

data Entity t
  = Entity
  { _entityId :: Unique
  , _entityPicture :: Dynamic t Picture
  , _entityWidth :: Width Float
  , _entityHeight :: Height Float
  , _entityPosition :: Dynamic t (V2 Float)
  , _entityQuadrants :: Dynamic t [Quadrant]
  }

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

mkMovingEntity
  :: ( MonadHold t m, MonadFix m
     , GridManager t (Entity t) m
     )
  => Unique
  -> Width Float
  -> Height Float
  -> Dynamic t Picture
  -> Dynamic t (V2 Float)
  -> m (Entity t)
mkMovingEntity
  _entityId
  _entityWidth
  _entityHeight
  _entityPicture
  _entityPosition = do

  _entityQuadrants <- getQuadrants _entityId

  let e = Entity{..}

  registerEntity
    _entityId
    e
    (_entityWidth, _entityHeight)
    _entityPosition

  pure e

class HasEntity s where
  entity :: Lens' (s t) (Entity t)

instance HasEntity Entity where
  entity = id

makeLensesFor
  [ ("_entityId", "entityId'")
  , ("_entityPicture", "entityPicture'")
  , ("_entityWidth", "entityWidth'")
  , ("_entityHeight", "entityHeight'")
  , ("_entityPosition", "entityPosition'")
  , ("_entityQuadrants", "entityQuadrants'")
  ]
  ''Entity

entityId :: Getter (Entity t) Unique
entityId = entityId'

entityPicture :: Getter (Entity t) (Dynamic t Picture)
entityPicture = entityPicture'

entityWidth :: Getter (Entity t) (Width Float)
entityWidth = entityWidth'

entityHeight :: Getter (Entity t) (Height Float)
entityHeight = entityHeight'

entityPosition :: Getter (Entity t) (Dynamic t (V2 Float))
entityPosition = entityPosition'

entityQuadrants :: Getter (Entity t) (Dynamic t [Quadrant])
entityQuadrants = entityQuadrants'

intersects
  :: Reflex t
  => (Dynamic t [Quadrant], Dynamic t (V2 Float), Width Float, Height Float)
  -> (Dynamic t [Quadrant], Dynamic t (V2 Float), Width Float, Height Float)
  -> Dynamic t Bool
intersects
  (dE1Quadrants, dE1Position, e1Width, e1Height)
  (dE2Quadrants, dE2Position, e2Width, e2Height) =

  (\e1Qs e2Qs e1Left e1Top e2Left e2Top ->
    let
      e1Right = e1Left + unWidth e1Width
      e1Bottom = e1Top + unHeight e1Height
      e2Right = e2Left + unWidth e2Width
      e2Bottom = e2Top + unHeight e2Height
    in
      any (`elem` e1Qs) e2Qs &&
      not
        (e1Right < e2Left ||
         e1Top > e2Bottom ||
         e1Left > e2Right ||
         e1Bottom < e2Top)) <$>
  dE1Quadrants <*>
  dE2Quadrants <*>
  ((^. _x) <$> dE1Position) <*>
  ((^. _y) <$> dE1Position) <*>
  ((^. _x) <$> dE2Position) <*>
  ((^. _y) <$> dE2Position)

mkStaticEntity
  :: ( MonadHold t m, Reflex t, MonadFix m
     , GridManager t (Entity t) m
     )
  => Map
  -> Unique
  -> Width Float
  -> Height Float
  -> V2 Float
  -> Dynamic t Picture
  -> m (Entity t)
mkStaticEntity mp eid ew eh epos pic =
  mkEntityPos mp ew eh epos never never >>=
  mkMovingEntity eid ew eh pic
