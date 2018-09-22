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
import Data.Foldable (foldMap, fold)
import Data.Map (Map)
import Data.Semigroup ((<>))
import Data.These (These)
import Graphics.Gloss (Display(..), Picture, pictures, blank, white)
import Graphics.Gloss.Juicy (loadJuicyPNG)
import System.Random (getStdGen)
import Linear.V2 (V2(..))

import qualified Data.Map as Map

import Controls (Controls(..), mkControls)
import Dimensions (Width(..), Height(..))
import Entity
  (Entity, mkMovingEntity, mkStaticEntity, entityQuadrants)
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

  eUnique <- requestUnique eCreate

  dBoxOpen <-
    mkBoxOpen
      playerIntersect
      (dBoxQuadrants, dBoxPos, bWidth, bHeight)
      ePlayerInteract

  let dBoxPicture = mkBoxPicture (openPic, closedPic) dBoxOpen

  (_, eBoxEntity) <-
    runWithReplace
      (pure ())
      ((\u -> mkStaticEntity mp u bWidth bHeight bPos dBoxPicture) <$>
       eUnique)

  dBoxQuadrants <- join <$> holdDyn (pure []) ((^.entityQuadrants) <$> eBoxEntity)

  pure (dBoxPicture, dBoxPos, bWidth, bHeight)

makeBox'
  :: ( Reflex t, MonadHold t m, MonadFix m
     , GridManager t (Entity t) m
     , Adjustable t m
     )
  => Game.Map
  -> Unique
  -> (Picture, Picture)
  -> Width Float
  -> Height Float
  -> V2 Float
  -> (Dynamic t [Quadrant], Dynamic t (V2 Float), Width Float, Height Float)
  -> Event t (V2 Float)
  -> m (Dynamic t Picture, Dynamic t (V2 Float), Width Float, Height Float)
makeBox' mp u (openPic, closedPic) bWidth bHeight bPos playerIntersect ePlayerInteract = mdo
  -- Make a box
  let dBoxPos = pure bPos

  dBoxOpen <-
    mkBoxOpen
      playerIntersect
      (dBoxQuadrants, dBoxPos, bWidth, bHeight)
      ePlayerInteract

  let dBoxPicture = mkBoxPicture (openPic, closedPic) dBoxOpen

  boxEntity <- mkStaticEntity mp u bWidth bHeight bPos dBoxPicture

  let dBoxQuadrants = boxEntity^.entityQuadrants

  pure (dBoxPicture, dBoxPos, bWidth, bHeight)

randomPosition
  :: (MonadHold t m, RandomGen t m)
  => Event t a
  -> (Int, Int)
  -> (Int, Int)
  -> m (Event t (V2 Float))
randomPosition eCreate xBounds yBounds = do
  eRandomInt1 <- fmap fromIntegral <$> randomIntR ((0, 1000) <$ eCreate)
  eRandomInt2 <- fmap fromIntegral <$> randomIntR ((0, 1000) <$ eCreate)
  switchHoldPromptly never ((\w -> V2 w <$> eRandomInt2) <$> eRandomInt1)

switchHoldUnique
  :: (MonadHold t m, UniqueSupply t m)
  => Event t a
  -> (Unique -> Event t b)
  -> m (Event t b)
switchHoldUnique eCreate f =
  requestUnique eCreate >>=
  switchHoldPromptly never . fmap f

makePlayer
  :: ( MonadHold t m, MonadFix m
     , UniqueSupply t m, GridManager t (Entity t) m
     , Adjustable t m
     )
  => Game.Map
  -> Controls t
  -> Event t a
  -> Picture
  -> Width Float
  -> Height Float
  -> V2 Float
  -> m ( Dynamic t [Quadrant]
       , Dynamic t Picture
       , Dynamic t (V2 Float)
       , Width Float
       , Height Float
       , Event t (V2 Float)
       )
makePlayer mp controls eCreate pic pWidth pHeight pPos = do
  -- Make a player
  dPlayerPos <- mkPlayerPos mp controls pWidth pHeight pPos
  let
    dPlayerPicture = pure pic
    ePlayerInteract = current dPlayerPos <@ _eSpacePressed controls

  eUnique <- requestUnique eCreate

  (_, ePlayerEntity) <-
    runWithReplace
      (pure ())
      ((\u -> mkMovingEntity u pWidth pHeight dPlayerPicture dPlayerPos) <$>
       eUnique)

  dPlayerQuadrants <-
    join <$> holdDyn (pure []) ((^.entityQuadrants) <$> ePlayerEntity)

  pure
    ( dPlayerQuadrants
    , dPlayerPicture
    , dPlayerPos
    , pWidth
    , pHeight
    , ePlayerInteract
    )

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

  (dPlayerQuadrants, dPlayerPicture, dPlayerPos, pWidth, pHeight, ePlayerInteract) <-
    makePlayer
      mp
      controls
      ePostBuild
      _assetsPlayerPicture
      (Width 20)
      (Height 20)
      (V2 0 0)

  (dBoxPicture, dBoxPos, bWidth, bHeight) <-
    makeBox
      mp
      ePostBuild
      (_assetsBoxOpenPicture, _assetsBoxClosedPicture)
      (Width 10)
      (Height 10)
      (V2 40 40)
      (dPlayerQuadrants, dPlayerPos, pWidth, pHeight)
      ePlayerInteract

  eInserts <-
    replicateM 5 $ do
      eRandomPos <- randomPosition ePostBuild (0, 990) (0, 990)
      switchHoldUnique ePostBuild (\u -> Map.singleton u . Just <$> eRandomPos)

  dBoxes <-
    listHoldWithKey
      mempty
      (fold eInserts)
      (\u pos ->
          makeBox'
            mp
            u
            (_assetsBoxOpenPicture, _assetsBoxClosedPicture)
            (Width 10)
            (Height 10)
            pos
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
