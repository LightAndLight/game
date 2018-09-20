{-# language FlexibleContexts #-}
{-# language FlexibleInstances, MultiParamTypeClasses #-}
{-# language GADTs, StandaloneDeriving #-}
{-# language RecursiveDo #-}
{-# language RecordWildCards #-}
{-# language TemplateHaskell #-}
{-# language ScopedTypeVariables #-}
module Main where

import Reflex
import Reflex.Gloss (InputEvent, playReflex)

import Control.Concurrent.Supply (newSupply)
import Control.Lens.Getter (uses, view)
import Control.Lens.Lens (Lens', lens)
import Control.Lens.Setter (assign)
import Control.Monad (replicateM, void, join)
import Control.Monad.Fix (MonadFix)
import Control.Monad.State.Strict
  (MonadState, evalStateT, get, put, StateT(..), runStateT)
import Data.Coerce (coerce)
import Data.Dependent.Sum (DSum(..))
import Data.Functor.Identity (Identity(..))
import Data.Functor.Misc (ComposeMaybe(..))
import Data.GADT.Compare ((:~:)(..), GOrdering(..), GEq(..), GCompare(..))
import Data.GADT.Show (GShow(..))
import Data.Semigroup ((<>))
import Data.Traversable (for)
import Graphics.Gloss (Display(..), Picture, pictures, white, blank, circle)
import Graphics.Gloss.Juicy (loadJuicyPNG)
import System.Random (Random, StdGen, getStdGen, next, randomR)
import Linear.V2 (V2(..))

import qualified Data.Dependent.Map as DMap

import Controls (mkControls)
import Dimensions (Width(..), Height(..))
import Entity (Entity, getMkEntity, entity, entityId)
import Entity.Box (Box, mkBox)
import Entity.Player (Player, mkPlayer)
import Grid (Grid)
import GridManager.Base (runGridManagerT)
import GridManager.Class (GridManager)
import Map (Map(..))
import RandomGen.Base (runRandomGenT)
import RandomGen.Class (RandomGen, randomInt)
import Render.Entity (renderedEntity, renderedEntityE)
import Render.Map (renderedMap, renderedMapE)
import Unique (Unique)
import UniqueSupply.Base (runUniqueSupplyT)
import UniqueSupply.Class (UniqueSupply(..))
import Viewport (ScreenSize(..), mkViewport, _vpWidth)

import Control.Lens (_1, over)

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

  eMkPlayerEntity <-
    fmap snd <$>
    getMkEntity
      ePostBuild
      mp
      _assetsPlayerPicture
      (Width 20)
      (Height 20)
      (V2 0 0)

  (_, ePlayer) <-
    runWithReplace
      (pure ())
      ((\mkE -> mkPlayer ePostBuild mkE controls) <$> eMkPlayerEntity)

  eMkBoxWithPlayer <-
    getMkEntity
      ePlayer
      mp
      _assetsBoxClosedPicture
      (Width 10)
      (Height 10)
      (V2 40 40)

  (_, eBox) <-
    runWithReplace
      (pure ())
      ((\(player, mbe) ->
          mkBox
            (() <$ ePlayer)
            mbe
            player
            (_assetsBoxClosedPicture, _assetsBoxOpenPicture)) <$>
        eMkBoxWithPlayer)

  eRandomInts <- replicateM 5 $ randomInt ePlayer

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

  (_, eViewport) <-
    runWithReplace
      (pure ())
      ((\p -> mkViewport 100 screenSize controls p mp) <$> ePlayer)

  renderedPlayer <- renderedEntityE eViewport ePlayer
  renderedBox <- renderedEntityE eViewport eBox
  renderedMap <- renderedMapE eViewport mp
  pure . fmap pictures . sequence $
    -- [ renderedMap viewport mp
    [ renderedMap
    , renderedPlayer
    , renderedBox
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
