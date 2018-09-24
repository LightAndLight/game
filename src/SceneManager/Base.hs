{-# language FlexibleInstances, MultiParamTypeClasses #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language RecursiveDo #-}
module SceneManager.Base where

import Reflex
import Reflex.NotReady.Class (NotReady(..))
import Control.Monad.Fix (MonadFix)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Trans.Class (MonadTrans(..))
import Data.Map (Map)
import Graphics.Gloss (Picture)

import qualified Data.Map as Map

import Entity (Entity)
import Render.Entity (renderedEntity)
import RandomGen.Class (RandomGen(..))
import SceneManager.Class (SceneManager(..))
import UniqueSupply.Class (UniqueSupply(..))
import Unique (Unique)
import Viewport (Viewport(..))

newtype SceneManagerT t m a
  = SceneManagerT
  { unSceneManagerT
    :: ReaderT
         (Dynamic t Picture)
         (EventWriterT t (Map Unique (Maybe (Entity t))) m)
         a
  } deriving
  ( Functor, Applicative, Monad, MonadFix
  , MonadSample t, MonadHold t
  )

instance MonadTrans (SceneManagerT t) where
  lift = SceneManagerT . lift . lift

instance PostBuild t m => PostBuild t (SceneManagerT t m) where
  getPostBuild = SceneManagerT getPostBuild

instance (Adjustable t m, MonadHold t m, MonadFix m) => Adjustable t (SceneManagerT t m) where
  runWithReplace a b = SceneManagerT $ runWithReplace (unSceneManagerT a) (unSceneManagerT <$> b)
  traverseIntMapWithKeyWithAdjust a b c =
    SceneManagerT $
    traverseIntMapWithKeyWithAdjust
      (\x y -> unSceneManagerT $ a x y)
      b
      c
  traverseDMapWithKeyWithAdjust a b c =
    SceneManagerT $
    traverseDMapWithKeyWithAdjust
      (\x y -> unSceneManagerT $ a x y)
      b
      c
  traverseDMapWithKeyWithAdjustWithMove a b c =
    SceneManagerT $
    traverseDMapWithKeyWithAdjustWithMove
      (\x y -> unSceneManagerT $ a x y)
      b
      c

instance NotReady t m => NotReady t (SceneManagerT t m) where
  notReadyUntil = lift . notReadyUntil
  notReady = lift notReady

runSceneManagerT
  :: ( Reflex t, MonadHold t m, MonadFix m
     , Adjustable t m
     )
  => Viewport t
  -> SceneManagerT t m a
  -> m a
runSceneManagerT vp (SceneManagerT m) = do
  rec
    (a, eUpdate) <-
      runEventWriterT . runReaderT m $
      dScene >>= foldMap (renderedEntity vp)
    dScene <- listHoldWithKey Map.empty eUpdate (\_ -> pure)
  pure a

instance (Reflex t, Monad m) => SceneManager t (SceneManagerT t m) where
  getScene = SceneManagerT ask
  addToScene = SceneManagerT . tellEvent

instance UniqueSupply t m => UniqueSupply t (SceneManagerT t m) where
  requestUnique = lift . requestUnique

instance RandomGen t m => RandomGen t (SceneManagerT t m) where
  randomInt = lift . randomInt
  randomIntR = lift . randomIntR
