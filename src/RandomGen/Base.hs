{-# language FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language RecursiveDo #-}
module RandomGen.Base where

import Reflex
import Control.Monad.Fix (MonadFix)
import Control.Monad.State (MonadState(..), evalState, gets)
import Control.Monad.Trans.Class (MonadTrans(..))
import Data.Functor.Const (Const(..))
import System.Random (StdGen, next)

import RandomGen.Class

instance (Reflex t, Monad m) => RandomGen t (RandomGenT t m) where
  randomInt = RandomGenT . fmap (fmap getConst) . requesting . (Const () <$)

newtype RandomGenT t m a
  = RandomGenT
  { unRandomGenT :: RequesterT t (Const ()) (Const Int) m a
  } deriving
  ( Functor, Applicative, Monad, MonadFix
  , MonadSample t, MonadHold t, PostBuild t
  )

instance MonadTrans (RandomGenT t) where
  lift = RandomGenT . lift

instance MonadState s m => MonadState s (RandomGenT t m) where
  get = lift get
  put = lift . put

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
  (a, eRequest) <- runRequesterT m eResponse
  let eResponse =
        flip evalState initialGen .
        traverseRequesterData
          (\_ -> do
              (n, s') <- gets next
              put s'
              pure $ Const n) <$>
        eRequest
  pure a
