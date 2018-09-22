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

import Dimensions (Width, Height)
import Entity
  ( ToEntity(..), HasQuadrants(..), HasPosition(..), HasPicture(..)
  , HasWidth(..), HasHeight(..)
  , mkStaticEntity, intersects
  )
import Graphics.Gloss (Picture)
import Grid (Quadrant)
import GridManager.Class (GridManager)
import Map (Map)
import Player (Player(..))
import UniqueSupply.Class (UniqueSupply, requestUnique)
import Unique (Unique)

data Box t
  = Box
  { _boxQuadrants :: Dynamic t [Quadrant]
  , _boxOpen :: Dynamic t Bool
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
     , UniqueSupply t m, GridManager t () m
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
mkBox mp eCreate (openPic, closedPic) _boxWidth _boxHeight bPos player = mdo
  let _boxPosition = pure bPos

  eUnique <- requestUnique eCreate

  _boxOpen <- mkBoxOpen player box

  let _boxPicture = mkBoxPicture (openPic, closedPic) _boxOpen

  (_, edQuadrants) <-
    runWithReplace
      (pure ())
      ((\u -> mkStaticEntity mp u _boxWidth _boxHeight bPos) <$>
       eUnique)

  _boxQuadrants <- join <$> holdDyn (pure []) edQuadrants

  let box = Box{..}

  pure box

mkBox'
  :: ( Reflex t, MonadHold t m, MonadFix m
     , GridManager t () m
     , Adjustable t m
     )
  => Map
  -> Unique
  -> (Picture, Picture)
  -> Width Float
  -> Height Float
  -> V2 Float
  -> Player t
  -> m (Box t)
mkBox' mp u (openPic, closedPic) _boxWidth _boxHeight bPos player = mdo
  let _boxPosition = pure bPos

  _boxOpen <- mkBoxOpen player box

  let _boxPicture = mkBoxPicture (openPic, closedPic) _boxOpen

  _boxQuadrants <- mkStaticEntity mp u _boxWidth _boxHeight bPos

  let box = Box{..}

  pure box

