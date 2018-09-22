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

import Box (Box(..), mkBox, mkBox')
import Controls (Controls(..), mkControls)
import Dimensions (Width(..), Height(..))
import Entity
  (Entity, mkMovingEntity, mkStaticEntity, entityQuadrants)
import Grid (Quadrant)
import GridManager.Base (runGridManagerT)
import GridManager.Class (GridManager)
import Player (Player(..), mkPlayer)
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

  player@Player{..} <-
    mkPlayer
      mp
      controls
      ePostBuild
      _assetsPlayerPicture
      (Width 20)
      (Height 20)
      (V2 0 0)

  box@Box{..} <-
    mkBox
      mp
      ePostBuild
      (_assetsBoxOpenPicture, _assetsBoxClosedPicture)
      (Width 10)
      (Height 10)
      (V2 40 40)
      (_playerQuadrants, _playerPosition, _playerWidth, _playerHeight)
      _playerInteract

  eInserts <-
    replicateM 5 $ do
      eRandomPos <- randomPosition ePostBuild (0, 990) (0, 990)
      switchHoldUnique ePostBuild (\u -> Map.singleton u . Just <$> eRandomPos)

  dBoxes <-
    listHoldWithKey
      mempty
      (fold eInserts)
      (\u pos ->
          mkBox'
            mp
            u
            (_assetsBoxOpenPicture, _assetsBoxClosedPicture)
            (Width 10)
            (Height 10)
            pos
            (_playerQuadrants, _playerPosition, _playerWidth, _playerHeight)
            _playerInteract)

  viewport <-
    mkViewport 100 screenSize mp controls (_playerWidth, _playerHeight, _playerPosition)

  let
    scene =
      fmap pictures . sequence $
      [ renderedMap viewport mp
      , renderedEntity viewport (_playerWidth, _playerHeight, _playerPosition) _playerPicture
      , renderedEntity viewport (_boxWidth, _boxHeight, _boxPosition) _boxPicture
      , dBoxes >>=
        foldMap
          (\Box{..} -> renderedEntity viewport (_boxWidth, _boxHeight, _boxPosition) _boxPicture)
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
