{-# language ConstraintKinds #-}
{-# language DefaultSignatures #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies, MultiParamTypeClasses #-}
{-# language LambdaCase #-}
{-# language RankNTypes #-}
{-# language RecordWildCards #-}
{-# language RecursiveDo #-}
{-# language TemplateHaskell #-}
module Entity where

import Control.Lens.Getter ((^.))
import Control.Lens.Lens (lens)
import Control.Lens.Prism (Prism')
import Control.Lens.Setter ((.~))
import Control.Lens.TH (makePrisms)
import Data.Function ((&))

import Box (Box)
import Chaser (Chaser)
import Dimensions (HasWidth(..), HasHeight(..))
import Entity.Picture (HasPicture(..))
import Entity.Position (HasPosition(..))
import Entity.Quadrants (HasQuadrants(..))
import Player (Player)

data Entity t
  = EntityPlayer (Player t)
  | EntityBox (Box t)
  | EntityChaser (Chaser t)
makePrisms ''Entity

class AsEntity t a | a -> t where
  _Entity :: Prism' (Entity t) a

instance AsEntity t (Entity t) where
  _Entity = id

instance AsEntity t (Box t) where
  _Entity = _EntityBox

instance AsEntity t (Player t) where
  _Entity = _EntityPlayer

instance AsEntity t (Chaser t) where
  _Entity = _EntityChaser

instance HasPicture t (Entity t) where
  picture =
    lens
      (\case
          EntityPlayer p -> p^.picture
          EntityChaser p -> p^.picture
          EntityBox b -> b^.picture)
      (\e pic ->
         case e of
           EntityPlayer p -> EntityPlayer $ p & picture .~ pic
           EntityChaser p -> EntityChaser $ p & picture .~ pic
           EntityBox p -> EntityBox $ p & picture .~ pic)

instance HasPosition t (Entity t) where
  position =
    lens
      (\case
          EntityPlayer p -> p^.position
          EntityChaser p -> p^.position
          EntityBox b -> b^.position)
      (\e pic ->
         case e of
           EntityPlayer p -> EntityPlayer $ p & position .~ pic
           EntityChaser p -> EntityChaser $ p & position .~ pic
           EntityBox p -> EntityBox $ p & position .~ pic)

instance HasQuadrants t (Entity t) where
  quadrants =
    lens
      (\case
          EntityPlayer p -> p^.quadrants
          EntityChaser p -> p^.quadrants
          EntityBox b -> b^.quadrants)
      (\e pic ->
         case e of
           EntityPlayer p -> EntityPlayer $ p & quadrants .~ pic
           EntityChaser p -> EntityChaser $ p & quadrants .~ pic
           EntityBox p -> EntityBox $ p & quadrants .~ pic)

instance HasWidth (Entity t) where
  width =
    lens
      (\case
          EntityPlayer p -> p^.width
          EntityChaser p -> p^.width
          EntityBox b -> b^.width)
      (\e pic ->
         case e of
           EntityPlayer p -> EntityPlayer $ p & width .~ pic
           EntityChaser p -> EntityChaser $ p & width .~ pic
           EntityBox p -> EntityBox $ p & width .~ pic)

instance HasHeight (Entity t) where
  height =
    lens
      (\case
          EntityPlayer p -> p^.height
          EntityChaser p -> p^.height
          EntityBox b -> b^.height)
      (\e pic ->
         case e of
           EntityPlayer p -> EntityPlayer $ p & height .~ pic
           EntityChaser p -> EntityChaser $ p & height .~ pic
           EntityBox p -> EntityBox $ p & height .~ pic)
