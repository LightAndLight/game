{-# language FlexibleContexts #-}
{-# language FlexibleInstances, MultiParamTypeClasses #-}
{-# language RecordWildCards #-}
{-# language RecursiveDo #-}
{-# language TemplateHaskell #-}
{-# language TupleSections #-}
module Box where

import Reflex
import Control.Lens.Operators ((<&>))
import Control.Lens.TH (makeLenses)
import Control.Monad.Fix (MonadFix)
import Data.Functor (($>))
import Linear.V2 (V2(..))

import Dimensions (Width(..), Height(..), HasWidth(..), HasHeight(..))
import Entity
  ( ToEntity(..), HasQuadrants(..)
  , HasPicture(..)
  , mkEntityPos, intersects
  )
import EntityStore.Class (EntityStore, tellEntity, quadrantsFor)
import Graphics.Gloss (Picture)
import Grid.Quadrant (Quadrant)
import Map (Map(..))
import Player (Player(..))
import Position (HasPosition(..))
import RandomGen.Class (RandomGen, randomPosition)
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

mkBoxAction
  :: ( Reflex t, MonadHold t m, MonadFix m
     , EntityStore t m, UniqueSupply t m, RandomGen t m, Adjustable t m
     )
  => Map
  -> (Picture, Picture)
  -> Width Float
  -> Height Float
  -> Player t
  -> Event t ()
  -> m ()
mkBoxAction mp pics w h player eOpened = do
  _ <- runWithReplace
    (pure ())
    (eOpened $> do
        eRandomPosition <-
          randomPosition
            (traceEventWith show eOpened)
            (0, floor . unWidth $ _mapWidth mp)
            (0, floor . unHeight $ _mapHeight mp)

        _ <- runWithReplace
          (pure ())
          (eRandomPosition <&>
            \(x, y) -> mkBox mp eRandomPosition pics w h (V2 x y) player)
        pure ())
  pure ()

mkBox
  :: ( Reflex t, MonadHold t m, MonadFix m
     , UniqueSupply t m, EntityStore t m, RandomGen t m
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

    tellEntity $ (, Just $ toEntity box) <$> eUnique
    _boxQuadrants <- quadrantsFor eUnique
    mkBoxAction
      mp
      (openPic, closedPic)
      _boxWidth _boxHeight
      player
      _boxOpenedFirstTime

    let box = Box{..}

  pure box

mkBox'
  :: ( Reflex t, MonadHold t m, MonadFix m
     , UniqueSupply t m, RandomGen t m, EntityStore t m
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

    tellEntity $ (u, Just $ toEntity box) <$ eCreate
    _boxQuadrants <- quadrantsFor (u <$ eCreate)
    mkBoxAction
      mp
      (openPic, closedPic)
      _boxWidth _boxHeight
      player
      _boxOpenedFirstTime

    let box = Box{..}

  pure box

