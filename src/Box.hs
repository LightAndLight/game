{-# language FlexibleContexts #-}
{-# language FlexibleInstances, MultiParamTypeClasses #-}
{-# language RecordWildCards #-}
{-# language RecursiveDo #-}
{-# language TemplateHaskell #-}
module Box where

import Reflex
import Control.Lens.TH (makeLenses)
import Control.Monad (join)
import Control.Monad.Fix (MonadFix)
import Linear.V2 (V2)

import qualified Data.Map as Map

import Dimensions (Width, Height, HasWidth(..), HasHeight(..))
import Entity
  ( Entity, ToEntity(..), HasQuadrants(..)
  , HasPicture(..)
  , mkEntity, mkEntityPos, intersects
  )
import Graphics.Gloss (Picture)
import Grid.Quadrant (Quadrant)
import GridManager.Class (GridManager)
import Map (Map)
import Player (Player(..))
import Position (HasPosition(..))
import SceneManager.Class (SceneManager, addToScene)
import UniqueSupply.Class (UniqueSupply, requestUnique)
import Unique (Unique)

data Box t
  = Box
  { _boxQuadrants :: Dynamic t [Quadrant]
  , _boxOpen :: Dynamic t Bool
  , _boxOpenedFirstTime :: Event t ()
  , _boxPicture :: Dynamic t Picture
  , _boxPosition :: Dynamic t (V2 Float)
  , _boxWidth :: Width Float
  , _boxHeight :: Height Float
  }
makeLenses ''Box

instance HasQuadrants t (Box t) where; quadrants = boxQuadrants
instance HasPosition t (Box t) where; position = boxPosition
instance HasPicture t (Box t) where; picture = boxPicture
instance HasWidth (Box t) where; width = boxWidth
instance HasHeight (Box t) where; height = boxHeight
instance ToEntity t (Box t)

mkBoxOpen
  :: (Reflex t, MonadHold t m, MonadFix m)
  => Player t
  -> Box t
  -> m (Dynamic t Bool)
mkBoxOpen player box = mdo
  dBoxOpen <-
    holdDyn False $
    fforMaybe
      ((,) <$>
        current dBoxOpen <*>
        current (intersects (toEntity player) (toEntity box)) <@ _playerInteract player)
      (\(open, touching) ->
          if touching
          then Just $ not open
          else Nothing)
  pure dBoxOpen

mkBoxPicture
  :: Reflex t
  => (Picture, Picture)
  -> Dynamic t Bool
  -> Dynamic t Picture
mkBoxPicture (open, closed) dBoxOpen =
  (\b -> if b then open else closed) <$> dBoxOpen

mkBox
  :: ( Reflex t, MonadHold t m, MonadFix m
     , UniqueSupply t m, GridManager t (Entity t) m
     , SceneManager t (Entity t) m
     , Adjustable t m
     )
  => Map
  -> Event t a
  -> (Picture, Picture)
  -> Width Float
  -> Height Float
  -> V2 Float
  -> Player t
  -> m (Box t)
mkBox mp eCreate (openPic, closedPic) _boxWidth _boxHeight bPos player = do
  _boxPosition <- mkEntityPos mp _boxWidth _boxHeight bPos never never

  eUnique <- requestUnique eCreate

  rec
    _boxOpen <- mkBoxOpen player box
    _boxOpenedFirstTime <- headE $ () <$ updated _boxOpen

    let _boxPicture = mkBoxPicture (openPic, closedPic) _boxOpen

    (_, edQuadrants) <-
      runWithReplace
        (pure ())
        ((\u -> do
             mkEntity u box) <$>
          eUnique)

    _boxQuadrants <- join <$> holdDyn (pure []) edQuadrants

    let box = Box{..}

  addToScene $ (\u -> Map.singleton u . Just $ toEntity box) <$> eUnique
  pure box

mkBox'
  :: ( Reflex t, MonadHold t m, MonadFix m
     , GridManager t (Entity t) m, SceneManager t (Entity t) m
     , Adjustable t m
     )
  => Map
  -> Unique
  -> Event t a
  -> (Picture, Picture)
  -> Width Float
  -> Height Float
  -> V2 Float
  -> Player t
  -> m (Box t)
mkBox' mp u eCreate (openPic, closedPic) _boxWidth _boxHeight bPos player = do
  _boxPosition <- mkEntityPos mp _boxWidth _boxHeight bPos never never

  rec
    _boxOpen <- mkBoxOpen player box
    _boxOpenedFirstTime <- headE $ () <$ updated _boxOpen

    let _boxPicture = mkBoxPicture (openPic, closedPic) _boxOpen

    _boxQuadrants <- mkEntity u box

    let box = Box{..}

  addToScene $ (Map.singleton u . Just $ toEntity box) <$ eCreate
  pure box

