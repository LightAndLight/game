{-# language FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
{-# language GADTs #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language RecursiveDo #-}
module RandomGen.Base where

import Reflex
import Reflex.NotReady.Class (NotReady(..))
import Control.Monad.Fix (MonadFix)
import Control.Monad.State (MonadState(..), runState, gets)
import Control.Monad.Trans.Class (MonadTrans(..))
import Data.Functor.Const (Const(..))
import System.Random (StdGen, next, randomR)

import RandomGen.Class

data RandomRequest a b where
  RandomInt :: RandomRequest () Int
  RandomIntR :: (Int, Int) -> RandomRequest () Int

instance (Reflex t, Monad m) => RandomGen t (RandomGenT t m) where
  randomInt = RandomGenT . fmap (fmap getConst) . requesting . (RandomInt <$)
  randomIntR = RandomGenT . fmap (fmap getConst) . requesting . (RandomIntR <$>)

newtype RandomGenT t m a
  = RandomGenT
  { unRandomGenT :: RequesterT t (RandomRequest ()) (Const Int) m a
  } deriving
  ( Functor, Applicative, Monad, MonadFix
  , MonadSample t, MonadHold t, PostBuild t
  )

instance MonadTrans (RandomGenT t) where
  lift = RandomGenT . lift

instance MonadState s m => MonadState s (RandomGenT t m) where
  get = lift get
  put = lift . put

instance NotReady t m => NotReady t (RandomGenT t m) where
  notReadyUntil = lift . notReadyUntil
  notReady = lift notReady

instance (Adjustable t m, MonadHold t m, MonadFix m) => Adjustable t (RandomGenT t m) where
  runWithReplace a b = RandomGenT $ runWithReplace (unRandomGenT a) (unRandomGenT <$> b)
  traverseIntMapWithKeyWithAdjust a b c =
    RandomGenT $
    traverseIntMapWithKeyWithAdjust
      (\x y -> unRandomGenT $ a x y)
      b
      c
  traverseDMapWithKeyWithAdjust a b c =
    RandomGenT $
    traverseDMapWithKeyWithAdjust
      (\x y -> unRandomGenT $ a x y)
      b
      c
  traverseDMapWithKeyWithAdjustWithMove a b c =
    RandomGenT $
    traverseDMapWithKeyWithAdjustWithMove
      (\x y -> unRandomGenT $ a x y)
      b
      c

runRandomGenT
  :: (Reflex t, MonadHold t m, MonadFix m)
  => StdGen
  -> RandomGenT t m a
  -> m a
runRandomGenT initialGen (RandomGenT m) = mdo
  bGen <- hold initialGen $ snd <$> eResponse
  (a, eRequest) <- runRequesterT m $ fst <$> eResponse
  let
    eResponse =
      (\g ->
          flip runState g .
          traverseRequesterData
          (\x -> do
              (n, s') <-
                gets $
                case x of
                  RandomInt -> next
                  RandomIntR r -> randomR r
              put s'
              pure $ Const n)) <$>
      bGen <@>
      eRequest
  pure a
