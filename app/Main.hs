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
import Graphics.Gloss (Display(..), Picture, pictures, white)
import Graphics.Gloss.Juicy (loadJuicyPNG)
import System.Random (getStdGen)
import Linear.V2 (V2(..))

import Controls (Controls(..), mkControls)
import Dimensions (Width(..), Height(..))
import Entity
  (Entity, mkMovingEntity, mkStaticEntity, getMkEntity, entityQuadrants)
import Entity.Box (mkBoxOpen, mkBoxPicture)
import Entity.Player (mkPlayerPos)
import GridManager.Base (runGridManagerT)
import GridManager.Class (GridManager)
import Map (Map(..))
import RandomGen.Base (runRandomGenT)
import RandomGen.Class (RandomGen, randomInt)
import Render.Entity (renderedEntity)
import Render.Map (renderedMap)
import UniqueSupply.Base (runUniqueSupplyT)
import UniqueSupply.Class (UniqueSupply(..))
import Viewport (ScreenSize(..), mkViewport)

data Assets
  = Assets
  { _assetsPlayerPicture :: Picture
  , _assetsMapPicture :: Picture
  , _assetsBoxClosedPicture :: Picture
  , _assetsBoxOpenPicture :: Picture
  }

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
    mp = Map _assetsMapPicture (Width 1000) (Height 1000)

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



  -- Make a box
  let
    bWidth = Width 10
    bHeight = Height 10
    bPos = V2 40 40
    dBoxPos = pure bPos

  eMkBoxEntity <-
    getMkEntity
      ePlayerEntity
      mp
      _assetsBoxClosedPicture
      bWidth
      bHeight
      bPos

  dBoxOpen <-
    mkBoxOpen
      (dPlayerQuadrants, dPlayerPos, bWidth, bHeight)
      (dBoxQuadrants, dBoxPos, pWidth, pHeight)
      ePlayerInteract

  let dBoxPicture = mkBoxPicture (_assetsBoxClosedPicture, _assetsBoxOpenPicture) dBoxOpen

  (_, eBoxEntity) <-
    runWithReplace
      (pure ())
      ((\mkE -> mkStaticEntity mkE dBoxPicture) <$> eMkBoxEntity)

  dBoxQuadrants <- join <$> holdDyn (pure []) ((^.entityQuadrants) <$> eBoxEntity)



  eRandomInts <- replicateM 5 $ randomInt ePlayerEntity

{-
  boxesCoords <-
    replicateM 5 $
    (,) <$>
    genRandomR 0 (_mapWidth mp - 10) <*>
    genRandomR 0 (_mapHeight mp - 10)

  boxes <-
    for boxesCoords $
    \(x, y) ->
      mkBox mp player (_assetsBoxClosedPicture, _assetsBoxOpenPicture) x y 10 10
  -}

  viewport <- mkViewport 100 screenSize mp controls (pWidth, pHeight, dPlayerPos)

  pure . fmap pictures . sequence $
    [ renderedMap viewport mp
    , renderedEntity viewport (pWidth, pHeight, dPlayerPos) dPlayerPicture
    , renderedEntity viewport (bWidth, bHeight, dBoxPos) dBoxPicture
    ]
    -- <> fmap (renderedEntity viewport) boxes

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
