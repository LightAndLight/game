{-# language FlexibleContexts #-}
{-# language FlexibleInstances, MultiParamTypeClasses #-}
{-# language RecursiveDo #-}
{-# language RecordWildCards #-}
module Main where

import Reflex
import Reflex.Gloss (InputEvent, playReflex)
import Graphics.Gloss (Display(..), Picture, pictures, white)
import Graphics.Gloss.Juicy (loadJuicyPNG)

import Control.Lens.Getter (uses)
import Control.Lens.Lens (Lens', lens)
import Control.Lens.Setter (assign)
import Control.Monad (replicateM)
import Control.Monad.Fix (MonadFix)
import Control.Monad.State.Strict (MonadState, evalStateT)
import Data.Semigroup ((<>))
import Data.Traversable (for)
import System.Random (Random, StdGen, getStdGen, next, randomR)

import Controls (mkControls)
import Entity (Entity)
import Entity.Box (mkBox)
import Entity.Player (mkPlayer)
import Grid (HasGrid(..), Grid, emptyGrid)
import Map (Map(..))
import Render.Entity (renderedEntity)
import Render.Map (renderedMap)
import Unique (Supply, HasSupply(..), newSupply)
import Viewport (ScreenSize(..), mkViewport)

data Assets
  = Assets
  { _assetsPlayerPicture :: Picture
  , _assetsMapPicture :: Picture
  , _assetsBoxClosedPicture :: Picture
  , _assetsBoxOpenPicture :: Picture
  }

class HasStdGen s where
  stdGen :: Lens' s StdGen

genRandom :: (HasStdGen s, MonadState s m) => m Int
genRandom = do
  (i, g) <- uses stdGen next
  assign stdGen g
  pure i

genRandomR :: (Random a, HasStdGen s, MonadState s m) => a -> a -> m a
genRandomR a b = do
  (i, g) <- uses stdGen $ randomR (a, b)
  assign stdGen g
  pure i

game
  :: ( Reflex t, MonadHold t m, MonadFix m
     , HasStdGen s, HasGrid s t (Entity t), HasSupply s, MonadState s m
     )
  => ScreenSize Float
  -> Assets
  -> Event t Float
  -> Event t InputEvent
  -> m (Behavior t Picture)
game screenSize Assets{..} refresh input = mdo
  controls <- mkControls refresh input

  let mp = Map _assetsMapPicture 1000 1000
  assign grid $ emptyGrid (1000, 1000)

  player <- mkPlayer controls mp _assetsPlayerPicture

  boxesCoords <-
    replicateM 5 $
    (,) <$>
    genRandomR 0 (_mapWidth mp - 10) <*>
    genRandomR 0 (_mapHeight mp - 10)

  boxes <-
    for boxesCoords $
    \(x, y) ->
      mkBox mp player (_assetsBoxClosedPicture, _assetsBoxOpenPicture) x y 10 10

  viewport <- mkViewport 100 screenSize controls player mp

  pure . fmap pictures . sequence $
    [ renderedMap viewport mp
    , renderedEntity viewport player
    ] <>
    fmap (renderedEntity viewport) boxes

data GameState t
  = GameState
  { gameGrid :: Grid t (Entity t)
  , gameStdGen :: StdGen
  , gameSupply :: Supply
  }

instance HasGrid (GameState t) t (Entity t) where
  grid = lens gameGrid (\gs g -> gs { gameGrid = g })

instance HasStdGen (GameState t) where
  stdGen = lens gameStdGen (\gs s -> gs { gameStdGen = s })

instance HasSupply (GameState t) where
  supply = lens gameSupply (\gs s -> gs { gameSupply = s })

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
       flip evalStateT (GameState (emptyGrid (0, 0)) stg sup) $
       game (fromIntegral <$> screenSize) Assets{..} er ei)
