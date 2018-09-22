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
import Control.Monad (replicateM, join)
import Control.Monad.Fix (MonadFix)
import Data.Foldable (foldMap, fold)
import Graphics.Gloss (Display(..), Picture, pictures, blank, white)
import Graphics.Gloss.Juicy (loadJuicyPNG)
import System.Random (getStdGen)
import Linear.V2 (V2(..))

import qualified Data.Map as Map

import Box (Box(..), mkBox, mkBox')
import Controls (mkControls)
import Dimensions (Width(..), Height(..))
import Entity (toEntity)
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
  eX <- fmap fromIntegral <$> randomIntR (xBounds <$ eCreate)
  eY <- fmap fromIntegral <$> randomIntR (yBounds <$ eCreate)
  switchHoldPromptly never ((\w -> V2 w <$> eY) <$> eX)

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
     , GridManager t () m, UniqueSupply t m, RandomGen t m
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
      player

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
            player)

  viewport <- mkViewport 100 screenSize mp controls player

  let
    scene =
      fmap pictures . sequence $
      [ renderedMap viewport mp
      , renderedEntity viewport $ toEntity player
      , renderedEntity viewport $ toEntity box
      , dBoxes >>= foldMap (renderedEntity viewport . toEntity)
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
