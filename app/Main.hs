{-# language FlexibleContexts #-}
{-# language FlexibleInstances, MultiParamTypeClasses #-}
{-# language GADTs #-}
{-# language RecursiveDo #-}
{-# language RecordWildCards #-}
{-# language ScopedTypeVariables #-}
module Main where

import Reflex
import Reflex.NotReady.Class (NotReady)
import Reflex.Gloss (InputEvent, playReflex)

import Control.Concurrent.Supply (newSupply)
import Control.Lens.Review ((#))
import Control.Lens.Setter (over, mapped)
import Control.Monad.Fix (MonadFix)
import Data.Foldable (fold)
import Data.Map (Map)
import Data.Semigroup ((<>))
import Graphics.Gloss (Display(..), Picture, white)
import Graphics.Gloss.Juicy (loadJuicyPNG)
import System.Random (getStdGen)
import Linear.V2 (V2(..))

import qualified Data.Map as Map

import Box (Box(..), initBox, initBoxes)
import Controls (mkControls)
import Dimensions (Width(..), Height(..))
import Entity (Entity, _Entity)
import Grid (GridConfig(..))
import Player (Player(..), mkPlayer)
import RandomGen.Base (runRandomGenT)
import RandomGen.Class (RandomGen)
import Render (render)
import Render.Map (renderedMap)
import Unique (Unique)
import UniqueSupply.Base (runUniqueSupplyT)
import UniqueSupply.Class (UniqueSupply(..))
import Viewport (ScreenSize(..), mkViewport, ViewportConfig(..))

import qualified Map as Game

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
     , UniqueSupply t m, RandomGen t m
     , NotReady t m
     )
  => ScreenSize Float
  -> Assets
  -> Event t Float
  -> Event t InputEvent
  -> m (Dynamic t Picture)
game screenSize Assets{..} refresh input = do
  let
    mp = Game.Map _assetsMapPicture (Width 1000) (Height 1000)
    gc = GridConfig 10 10 (Width 1000) (Height 1000)

  controls <- mkControls refresh input

  ePostBuild <- getPostBuild


  ePlayerCreated :: Event t Unique <- requestUnique ePostBuild

  player :: Player t <-
    mkPlayer
      mp
      gc
      controls
      _assetsPlayerPicture
      (Width 20)
      (Height 20)
      (V2 0 0)

  let
    ePlayerUpdated = (\u -> Map.singleton u $ Just player) <$> ePlayerCreated


  eInitialBox <-
    initBox
      ePostBuild
      mp
      gc
      (_assetsBoxOpenPicture, _assetsBoxClosedPicture)
      player
      (Width 10)
      (Height 10)
      (V2 40 40)

  eBoxesUpdated :: Event t (Map Unique (Maybe (Box t))) <- initBoxes eInitialBox


  dEntities :: Dynamic t (Map Unique (Entity t)) <-
    listHoldWithKey
      mempty
      (over (mapped.mapped.mapped) (_Entity #) eBoxesUpdated <>
        over (mapped.mapped.mapped) (_Entity #) ePlayerUpdated)
      (\_ -> pure)

  viewport <- mkViewport screenSize mp [EdgePan 100 player]

  pure $
    fold
    [ renderedMap viewport mp
    , render viewport dEntities
    ]

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

  let
    screenWidth = 600
    screenHeight = 400
    screenSize = ScreenSize screenWidth screenHeight

  stg <- getStdGen
  sup <- newSupply

  playReflex
    (InWindow "game" (screenWidth, screenHeight) (0, 0))
    white
    30
    (\er ei ->
       runRandomGenT stg $
       runUniqueSupplyT sup $
       game (fromIntegral <$> screenSize) Assets{..} er ei)
