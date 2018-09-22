{-# language FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language RecursiveDo #-}
{-# language ScopedTypeVariables #-}
module GridManager.Base where

import Reflex
import Control.Monad.Fix (MonadFix)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.State (MonadState(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Linear.V2 (V2(..))

import Dimensions (Width(..), Height(..))
import Grid (Grid(..))
import GridManager.Class (GridManager(..))
import UniqueSupply.Class (UniqueSupply(..))
import RandomGen.Class (RandomGen(..))
import Unique (Unique)
import UniqueMap (UniqueMap)

import qualified UniqueMap

newtype GridManagerT t g m a
  = GridManagerT
  { unGridManagerT
    :: ReaderT
         (Grid t g)
         (DynamicWriterT t
            (UniqueMap (g, (Width Float, Height Float), V2 Float))
            m)
         a
  } deriving
  ( Functor, Applicative, Monad, MonadFix
  , MonadSample t, MonadHold t, PostBuild t
  )

instance (Adjustable t m, MonadHold t m, MonadFix m) => Adjustable t (GridManagerT t g m) where
  runWithReplace a b = GridManagerT $ runWithReplace (unGridManagerT a) (unGridManagerT <$> b)
  traverseIntMapWithKeyWithAdjust a b c =
    GridManagerT $
    traverseIntMapWithKeyWithAdjust
      (\x y -> unGridManagerT $ a x y)
      b
      c
  traverseDMapWithKeyWithAdjust a b c =
    GridManagerT $
    traverseDMapWithKeyWithAdjust
      (\x y -> unGridManagerT $ a x y)
      b
      c
  traverseDMapWithKeyWithAdjustWithMove a b c =
    GridManagerT $
    traverseDMapWithKeyWithAdjustWithMove
      (\x y -> unGridManagerT $ a x y)
      b
      c

runGridManagerT
  :: forall t g m a
   . (Reflex t, MonadHold t m, MonadFix m)
  => Width Float
  -> Height Float
  -> GridManagerT t g m a
  -> m a
runGridManagerT w h (GridManagerT m) = mdo
  let
    grid :: Grid t g
    grid = Grid w h vc hc dTopLeft dTopRight dBottomLeft dBottomRight

  (a, dItems) <- runDynamicWriterT (runReaderT m grid)

  let
    dTopLeft = filterQuadrant inTopLeft dItems
    dTopRight = filterQuadrant inTopRight dItems
    dBottomLeft = filterQuadrant inBottomLeft dItems
    dBottomRight = filterQuadrant inBottomRight dItems

  pure a

  where
    filterQuadrant test =
      fmap $
      UniqueMap.mapMaybe
        (\(g, dims, p) -> if test dims p then Just g else Nothing)

    vc = unWidth w/2
    hc = unHeight h/2

    checkBounds (Width aW, Height aH) (V2 x y) =
      (x < vc, y < hc, x + aW < vc, y + aH < hc)

    inTopLeft dims p =
      case checkBounds dims p of
         -- top left
         (True, True, False, False) -> -- leaks into right half and bottom half
           True
         (True, True, False, True) -> -- leaks into right half but not bottom half
           True
         (True, True, True, False) -> -- leaks into bottom half but not right half
           True
         (True, True, True, True) -> -- leaks into neither right half not bottom half
           True
         _ -> False

    inTopRight dims p =
      case checkBounds dims p of
          -- top left
          (True, True, False, False) -> -- leaks into right half and bottom half
            True
          (True, True, False, True) -> -- leaks into right half but not bottom half
            True
          -- top right
          (False, True, _, False) -> -- leaks into bottom half
            True
          (False, True, _, True) -> -- doesn't leak into bottom half
            True
          _ -> False

    inBottomLeft dims p =
       case checkBounds dims p of
          -- top left
          (True, True, False, False) -> -- leaks into right half and bottom half
            True
          (True, True, True, False) -> -- leaks into bottom half but not right half
            True
          -- bottom left
          (True, False, False, _) -> -- leaks into right half
            True
          (True, False, True, _) -> -- doesn't leak into right half
            True
          _ -> False

    inBottomRight dims p =
      case checkBounds dims p of
          -- top left
          (True, True, False, False) -> -- leaks into right half and bottom half
            True
          -- bottom left
          (True, False, False, _) -> -- leaks into right half
            True
          -- top right
          (False, True, _, False) -> -- leaks into bottom half
            True
          -- bottom right
          (False, False, _, _) ->
            True
          _ -> False

instance MonadTrans (GridManagerT t g) where
  lift = GridManagerT . lift . lift

instance (Reflex t, Monad m) => GridManager t g (GridManagerT t g m) where
  getGrid = GridManagerT ask
  registerEntity = registerEntityImpl

instance UniqueSupply t m => UniqueSupply t (GridManagerT t g m) where
  requestUnique = lift . requestUnique

instance RandomGen t m => RandomGen t (GridManagerT t g m) where
  randomInt = lift . randomInt
  randomIntR = lift . randomIntR

instance MonadState s m => MonadState s (GridManagerT t g m) where
  get = lift get
  put = lift . put

registerEntityImpl
  :: (Reflex t, Monad m)
  => Unique
  -> g
  -> (Width Float, Height Float)
  -> Dynamic t (V2 Float)
  -> GridManagerT t g m ()
registerEntityImpl a b c d =
  GridManagerT $ tellDyn $ UniqueMap.singleton a . (,,) b c <$> d
