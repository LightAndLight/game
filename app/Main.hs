{-# language FlexibleContexts #-}
{-# language FlexibleInstances, MultiParamTypeClasses #-}
{-# language GADTs #-}
{-# language RecursiveDo #-}
{-# language RecordWildCards #-}
{-# language ScopedTypeVariables #-}
module Main where

import Reflex
import Reflex.Gloss (InputEvent, playReflex)

import Control.Concurrent.Supply (newSupply)
import Control.Lens.Getter ((^.))
import Control.Monad (replicateM, join)
import Control.Monad.Fix (MonadFix)
import Data.Foldable (foldMap)
import Data.Map (Map)
import Data.These (These)
import Graphics.Gloss (Display(..), Picture, pictures, blank, white)
import Graphics.Gloss.Juicy (loadJuicyPNG)
import System.Random (getStdGen)
import Linear.V2 (V2(..))

import qualified Data.Map as Map

import Controls (Controls(..), mkControls)
import Dimensions (Width(..), Height(..))
import Entity
  (Entity, mkMovingEntity, mkStaticEntity, getMkEntity, entityQuadrants)
import Entity.Box (mkBoxOpen, mkBoxPicture)
import Entity.Player (mkPlayerPos)
import Grid (Quadrant)
import GridManager.Base (runGridManagerT)
import GridManager.Class (GridManager)
import RandomGen.Base (runRandomGenT)
import RandomGen.Class (RandomGen, randomIntR)
import Render.Entity (renderedEntity)
import Render.Map (renderedMap)
import Unique (Unique)
import UniqueSupply.Base (runUniqueSupplyT)
import UniqueSupply.Class (UniqueSupply(..))
import Viewport (ScreenSize(..), mkViewport)

import qualified Map as Game

data Assets
  = Assets
  { _assetsPlayerPicture :: Picture
  , _assetsMapPicture :: Picture
  , _assetsBoxClosedPicture :: Picture
  , _assetsBoxOpenPicture :: Picture
  }

makeBox
  :: ( Reflex t, MonadHold t m, MonadFix m
     , UniqueSupply t m, GridManager t (Entity t) m
     , Adjustable t m
     )
  => Game.Map
  -> Event t a
  -> (Picture, Picture)
  -> Width Float
  -> Height Float
  -> V2 Float
  -> (Dynamic t [Quadrant], Dynamic t (V2 Float), Width Float, Height Float)
  -> Event t (V2 Float)
  -> m (Dynamic t Picture, Dynamic t (V2 Float), Width Float, Height Float)
makeBox mp eCreate (openPic, closedPic) bWidth bHeight bPos playerIntersect ePlayerInteract = mdo
  -- Make a box
  let
    dBoxPos = pure bPos

  eMkBoxEntity <-
    getMkEntity
      eCreate
      mp
      closedPic
      bWidth
      bHeight
      bPos

  dBoxOpen <-
    mkBoxOpen
      playerIntersect
      (dBoxQuadrants, dBoxPos, bWidth, bHeight)
      ePlayerInteract

  let dBoxPicture = mkBoxPicture (openPic, closedPic) dBoxOpen

  (_, eBoxEntity) <-
    runWithReplace
      (pure ())
      ((\mkE -> mkStaticEntity mkE dBoxPicture) <$> eMkBoxEntity)

  dBoxQuadrants <- join <$> holdDyn (pure []) ((^.entityQuadrants) <$> eBoxEntity)

  pure (dBoxPicture, dBoxPos, bWidth, bHeight)

game
  :: forall t m
   . ( Reflex t, MonadHold t m, MonadFix m
     , PostBuild t m, Adjustable t m
     , GridManager t (Entity t) m, UniqueSupply t m, RandomGen t m
     )
  => ScreenSize Float
  -> Assets
  -> Event t Float
  -> Event t InputEvent
  -> m (Dynamic t Picture)
game screenSize Assets{..} refresh input = mdo
  controls <- mkControls refresh input

  let
    mp = Game.Map _assetsMapPicture (Width 1000) (Height 1000)

  ePostBuild <- getPostBuild


  -- Make a player
  let
    pWidth = Width 20
    pHeight = Height 20
    pPos = V2 0 0

  dPlayerPos <- mkPlayerPos mp controls pWidth pHeight pPos
  let
    dPlayerPicture = pure _assetsPlayerPicture
    ePlayerInteract = current dPlayerPos <@ _eSpacePressed controls

  eMkPlayerEntity <-
    getMkEntity
      ePostBuild
      mp
      _assetsPlayerPicture
      pWidth
      pHeight
      pPos

  (_, ePlayerEntity) <-
    runWithReplace
      (pure ())
      ((\mkE -> mkMovingEntity mkE dPlayerPicture dPlayerPos) <$> eMkPlayerEntity)

  dPlayerQuadrants <- join <$> holdDyn (pure []) ((^.entityQuadrants) <$> ePlayerEntity)

  (dBoxPicture, dBoxPos, bWidth, bHeight) <-
    makeBox
      mp
      ePlayerEntity
      (_assetsBoxOpenPicture, _assetsBoxClosedPicture)
      (Width 10)
      (Height 10)
      (V2 40 40)
      (dPlayerQuadrants, dPlayerPos, pWidth, pHeight)
      ePlayerInteract

  eRandomInt1 <- fmap fromIntegral <$> randomIntR ((0, 1000) <$ ePlayerEntity)
  eRandomInt2 <- fmap fromIntegral <$> randomIntR ((0, 1000) <$ ePlayerEntity)
  eRandomPos <- switchHoldPromptly never ((\w -> (,) w <$> eRandomInt2) <$> eRandomInt1)

  let eInsert = Map.singleton 0 . Just <$> eRandomPos

  dBoxes <-
    listHoldWithKey
      (mempty :: Map Int (Float, Float))
      eInsert
      (\_ (x, y) ->
          makeBox
            mp
            eInsert
            (_assetsBoxOpenPicture, _assetsBoxClosedPicture)
            (Width 10)
            (Height 10)
            (V2 x y)
            (dPlayerQuadrants, dPlayerPos, pWidth, pHeight)
            ePlayerInteract)

  viewport <- mkViewport 100 screenSize mp controls (pWidth, pHeight, dPlayerPos)

  let
    scene =
      fmap pictures . sequence $
      [ renderedMap viewport mp
      , renderedEntity viewport (pWidth, pHeight, dPlayerPos) dPlayerPicture
      , renderedEntity viewport (bWidth, bHeight, dBoxPos) dBoxPicture
      , dBoxes >>=
        foldMap
          (\(dPic, dPos, w, h) -> renderedEntity viewport (w, h, dPos) dPic)
      ]
  join <$> holdDyn (pure blank) (scene <$ ePostBuild)

main :: IO ()
main = do
  _assetsPlayerPicture <-
    maybe (error "couldn't load dude") pure =<<
    loadJuicyPNG "assets/dude.png"

  _assetsMapPicture <-
    maybe (error "couldn't load map") pure =<<
    loadJuicyPNG "assets/map.png"

  _assetsBoxClosedPicture <-
    maybe (error "couldn't load box_closed") pure =<<
    loadJuicyPNG "assets/box_closed.png"

  _assetsBoxOpenPicture <-
    maybe (error "couldn't load box_open") pure =<<
    loadJuicyPNG "assets/box_open.png"

  let screenSize = ScreenSize (600, 400)

  stg <- getStdGen
  sup <- newSupply

  playReflex
    (InWindow "game" (unScreenSize screenSize) (0, 0))
    white
    30
    (\er ei ->
       runRandomGenT stg $
       runUniqueSupplyT sup $
       runGridManagerT (Width 1000) (Height 1000) $
       game (fromIntegral <$> screenSize) Assets{..} er ei)
