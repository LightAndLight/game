{-# language FlexibleContexts #-}
{-# language RecordWildCards #-}
{-# language RecursiveDo #-}
{-# language TemplateHaskell #-}
module Entity
  ( Entity
  , MkEntity
  , getMkEntity
  , mkStaticEntity
  , mkMovingEntity
  , entityId
  , entityPicture
  , entityWidth
  , entityHeight
  , entityPosition
  , entityQuadrants
  , intersects
  , intersectsEE
  , HasEntity(..)
  )
where

import Reflex

import Control.Lens.Getter ((^.), Getter, to)
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
import UniqueSupply.Class (UniqueSupply, requestUnique)

data MkEntity
  = MkEntity
  { _mkEntityId :: Unique
  , _mkEntityPicture :: Picture
  , _mkEntityWidth :: Width Float
  , _mkEntityHeight :: Height Float
  , _mkEntityPosition :: V2 Float
  , _mkEntityMap :: Map
  }

data Entity t
  = Entity
  { _entityId :: Unique
  , _entityPicture :: Dynamic t Picture
  , _entityWidth :: Width Float
  , _entityHeight :: Height Float
  , _entityPosition :: Dynamic t (V2 Float)
  , _entityQuadrants :: Dynamic t [Quadrant]
  }

getMkEntity
  :: UniqueSupply t m
  => Event t a -- ^ event which causes entity construction
  -> Map -- ^ initial picture
  -> Picture -- ^ initial picture
  -> Width Float -- ^ width
  -> Height Float -- ^ height
  -> V2 Float -- ^ (x, y) coordinate
  -> m (Event t (a, MkEntity))
getMkEntity eBuild mp a b d e = do
  eUnique <- requestUnique eBuild
  let eResult = coincidence $ (\u -> (,) u <$> eBuild) <$> eUnique
  pure $ (\(u, res) -> (res, MkEntity u a b d e mp)) <$> eResult

mkMovingEntity
  :: ( MonadHold t m, MonadFix m
     , GridManager t (Entity t) m
     )
  => Event t ()
  -> MkEntity
  -> Event t Picture
  -> Event t Float -- ^ when x coordinate changed
  -> Event t Float -- ^ when y coordinate changed
  -> m (Entity t)
mkMovingEntity eAdd MkEntity{..} ePicture eX eY = do
  _entityPicture <- holdDyn _mkEntityPicture ePicture

  dX <-
    holdUniqDyn =<<
    holdDyn
      (_mkEntityPosition^._x)
      (max 0 . min (unWidth (_mapWidth _mkEntityMap) - unWidth _mkEntityWidth / 2) <$> eX)

  dY <-
    holdUniqDyn =<<
    holdDyn
      (_mkEntityPosition^._y)
      (max 0 . min (unHeight (_mapHeight _mkEntityMap) - unHeight _mkEntityHeight / 2) <$> eY)

  let
    _entityId = _mkEntityId
    _entityWidth = _mkEntityWidth
    _entityHeight = _mkEntityHeight
    _entityPosition = V2 <$> dX <*> dY

  _entityQuadrants <- getQuadrants _entityId

  let e = Entity{..}

  registerEntity
    _entityId
    e
    (_entityWidth, _entityHeight)
    (updated _entityPosition)

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

intersectsEE
  :: ( Reflex t, MonadHold t m
     , HasEntity a, HasEntity b
     )
  => Event t (a t)
  -> Event t (b t)
  -> m (Behavior t Bool)
intersectsEE a b = do
  e1Position <-
    switcher (pure $ V2 0 0) $ current . (^.entity.entityPosition) <$> a
  e1Quadrants <-
    switcher (pure []) $ current. (^.entity.entityQuadrants) <$> a
  e1Width <- hold 0 $ (^.entity.entityWidth.to unWidth) <$> a
  e1Height <- hold 0 $ (^.entity.entityHeight.to unHeight) <$> a

  e2Position <-
    switcher (pure $ V2 0 0) $ current . (^.entity.entityPosition) <$> b
  e2Quadrants <-
    switcher (pure []) $ current . (^.entity.entityQuadrants) <$> b
  e2Width <- hold 0 $ (^.entity.entityWidth.to unWidth) <$> b
  e2Height <- hold 0 $ (^.entity.entityHeight.to unHeight) <$> b

  pure $
    (\e1W e1H e2W e2H e1Qs e2Qs e1Left e1Top e2Left e2Top ->
      let
        e1Right = e1Left + e1W
        e1Bottom = e1Top + e1H
        e2Right = e2Left + e2W
        e2Bottom = e2Top + e2H
      in
        any (`elem` e1Qs) e2Qs &&
        not
          (e1Right < e2Left ||
            e1Top > e2Bottom ||
            e1Left > e2Right ||
            e1Bottom < e2Top)) <$>
    e1Width <*>
    e1Height <*>
    e2Width <*>
    e2Height <*>
    e1Quadrants <*>
    e2Quadrants <*>
    ((^. _x) <$> e1Position) <*>
    ((^. _y) <$> e1Position) <*>
    ((^. _x) <$> e2Position) <*>
    ((^. _y) <$> e2Position)

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
        e1Right = e1Left + e1^.entityWidth.to unWidth
        e1Bottom = e1Top + e1^.entityHeight.to unHeight
        e2Right = e2Left + e2^.entityWidth.to unWidth
        e2Bottom = e2Top + e2^.entityHeight.to unHeight
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

mkStaticEntity
  :: ( MonadHold t m, Reflex t, MonadFix m
     , GridManager t (Entity t) m
     )
  => Event t ()
  -> MkEntity
  -> Event t Picture -- ^ when the picture changes
  -> m (Entity t)
mkStaticEntity eAdd mkE pic = mkMovingEntity eAdd mkE pic never never
