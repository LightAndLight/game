{-# language FlexibleContexts #-}
{-# language FlexibleInstances, MultiParamTypeClasses #-}
{-# language GADTs #-}
{-# language RecursiveDo #-}
{-# language RecordWildCards #-}
{-# language ScopedTypeVariables #-}
module Main where

import Reflex
import Reflex.Network (networkView)
import Reflex.NotReady.Class (NotReady)
import Reflex.Gloss (InputEvent, playReflex)
import Reflex.Workflow (Workflow(..), workflow)

import Control.Concurrent.Supply (newSupply)
import Control.Lens.Getter (view)
import Control.Monad (join)
import Control.Monad.Fix (MonadFix)
import Data.Foldable (foldMap, fold)
import Data.Map (Map)
import Data.Semigroup ((<>))
import Graphics.Gloss (Display(..), Picture, pictures, blank, white)
import Graphics.Gloss.Juicy (loadJuicyPNG)
import System.Random (getStdGen)
import Linear.V2 (V2(..))

import qualified Data.Map as Map

import Box (Box(..), mkBox, mkBox', boxOpenedFirstTime)
import Controls (mkControls)
import Dimensions (Width(..), Height(..))
import Entity (Entity, toEntity)
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
import Viewport (ScreenSize(..), mkViewport, ViewportConfig(..))

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

mkUniqueAndPos
  :: ( MonadHold t m
     , UniqueSupply t m, RandomGen t m
     )
  => Event t a
  -> m (Event t (Map Unique (Maybe (V2 Float))))
mkUniqueAndPos eCreate = do
  eRandomPos <- randomPosition eCreate (0, 990) (0, 990)
  switchHoldUnique eCreate (\u -> Map.singleton u . Just <$> eRandomPos)

mkUniqueAndPosNotOnPlayer
  :: ( Reflex t, MonadHold t m, MonadFix m
     , UniqueSupply t m, RandomGen t m
     , Adjustable t m
     )
  => Dynamic t (V2 Float)
  -> Event t a
  -> m (Event t (Map Unique (Maybe (V2 Float))))
mkUniqueAndPosNotOnPlayer dPlayerPos eCreate = switchDyn <$> workflow w
  where
    w = Workflow $ do
      eRandomPos <- mkUniqueAndPos eCreate
      let
        eRetry =
          ffilter id
          ((\a -> any $ maybe False (a ==)) <$>
            current dPlayerPos <@>
            eRandomPos)
      pure (eRandomPos, w <$ eRetry)

game
  :: forall t m
   . ( Reflex t, MonadHold t m, MonadFix m
     , PostBuild t m, Adjustable t m
     , GridManager t (Entity t) m, UniqueSupply t m, RandomGen t m
     , NotReady t m
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

  eInitial <- mkUniqueAndPosNotOnPlayer _playerPosition _boxOpenedFirstTime
  eLater <-
    networkView $
      fmap fold .
      traverse
        (mkUniqueAndPosNotOnPlayer _playerPosition .
        view boxOpenedFirstTime) <$>
      dBoxes
  eInsert <- switchHold never eLater

  dBoxes :: Dynamic t (Map Unique (Box t)) <-
    listHoldWithKey
      mempty
      (eInitial <> eInsert)
      (\u pos ->
          mkBox'
            mp
            u
            (_assetsBoxOpenPicture, _assetsBoxClosedPicture)
            (Width 10)
            (Height 10)
            pos
            player)

  viewport <- mkViewport screenSize mp [EdgePan 100 $ toEntity player]

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
       runGridManagerT 2 2 (Width 1000) (Height 1000) $
       game (fromIntegral <$> screenSize) Assets{..} er ei)
