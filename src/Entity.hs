{-# language FlexibleContexts #-}
{-# language RecordWildCards #-}
{-# language RecursiveDo #-}
{-# language TemplateHaskell #-}
module Entity
  ( Entity
  , mkEntity
  , entityId
  , entityPicture
  , entityWidth
  , entityHeight
  , entityPosition
  , entityQuadrants
  , mkStaticEntity
  , mkMovingEntity
  , intersects
  , HasEntity(..)
  )
where

import Reflex

import Control.Lens.Getter ((^.), Getter, use)
import Control.Lens.Lens (Lens')
import Control.Lens.Setter (assign)
import Control.Lens.TH (makeLensesFor)
import Control.Monad.Fix (MonadFix)
import Control.Monad.State.Strict (MonadState)
import Graphics.Gloss (Picture)
import Linear.V2 (V2(..), R1(..), R2(..))

import Grid (HasGrid, Quadrant, insertG, grid)
import Map (Map(..))
import Unique (Unique, HasSupply, getUnique)

data Entity t
  = Entity
  { _entityId :: Unique
  , _entityPicture :: Behavior t Picture
  , _entityWidth :: Float
  , _entityHeight :: Float
  , _entityPosition :: Dynamic t (V2 Float)
  , _entityQuadrants :: Dynamic t [Quadrant]
  }

mkEntity
  :: (HasSupply s, MonadState s m)
  => Behavior t Picture
  -> Float
  -> Float
  -> Dynamic t (V2 Float)
  -> Dynamic t [Quadrant]
  -> m (Entity t)
mkEntity a b c d e = (\f -> Entity f a b c d e) <$> getUnique

mkStaticEntity
  :: ( MonadHold t m, Reflex t, MonadFix m
     , HasGrid s t (Entity t), HasSupply s, MonadState s m
     )
  => Map -- ^ map on which the entity resides
  -> Behavior t Picture
  -> Float -- ^ x coordinate
  -> Float -- ^ y coordinate
  -> Float -- ^ width
  -> Float -- ^ height
  -> m (Entity t)
mkStaticEntity Map{..} pic x y w h = mdo
  let
    pos =
      pure $
      V2
        (max 0 $ min (_mapWidth - w) x)
        (max 0 $ min (_mapHeight - h) y)

  entity <- mkEntity pic w h pos qdrs

  (g', qdrs) <- insertG entity (x, y) (w, h) never =<< use grid
  assign grid g'

  pure entity

mkMovingEntity
  :: ( MonadHold t m, Reflex t, MonadFix m
     , HasGrid s t (Entity t), HasSupply s, MonadState s m
     )
  => Map -- ^ map on which the entity resides
  -> Behavior t Picture
  -> Dynamic t Float -- ^ x coordinate
  -> Dynamic t Float -- ^ y coordinate
  -> Float -- ^ width
  -> Float -- ^ height
  -> m (Entity t)
mkMovingEntity Map{..} pic x y w h = mdo
  let
    pos =
      V2 <$>
      fmap (max 0 . min (_mapWidth - w)) x <*>
      fmap (max 0 . min (_mapHeight - h)) y

  entity <- mkEntity pic w h pos qdrs

  (g', qdrs) <-
    insertG entity (0, 0) (w, h) (updated pos) =<<
    use grid

  assign grid g'

  pure entity

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

entityPicture :: Getter (Entity t) (Behavior t Picture)
entityPicture = entityPicture'

entityWidth :: Getter (Entity t) Float
entityWidth = entityWidth'

entityHeight :: Getter (Entity t) Float
entityHeight = entityHeight'

entityPosition :: Getter (Entity t) (Dynamic t (V2 Float))
entityPosition = entityPosition'

entityQuadrants :: Getter (Entity t) (Dynamic t [Quadrant])
entityQuadrants = entityQuadrants'

intersects
  :: (Reflex t, HasEntity a, HasEntity b)
  => a t
  -> b t
  -> Dynamic t Bool
intersects a b =
  let
    e1 = a ^. entity
    e2 = b ^. entity
  in
    (\e1Qs e2Qs e1Left e1Top e2Left e2Top ->
      let
        e1Right = e1Left + e1^.entityWidth
        e1Bottom = e1Top + e1^.entityWidth
        e2Right = e2Left + e2^.entityWidth
        e2Bottom = e2Top + e2^.entityWidth
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
