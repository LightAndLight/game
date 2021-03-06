{-# language FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language RecursiveDo #-}
module UniqueSupply.Base where

import Reflex
import Reflex.NotReady.Class (NotReady(..))
import Control.Concurrent.Supply (Supply, freshId)
import Control.Monad.Fix (MonadFix)
import Control.Monad.State (MonadState(..), runState, gets)
import Control.Monad.Trans.Class (MonadTrans(..))
import Data.Functor.Const (Const(..))

import RandomGen.Class (RandomGen(..))
import Unique
import UniqueSupply.Class

newtype UniqueSupplyT t m a
  = UniqueSupplyT
  { unUniqueSupplyT :: RequesterT t (Const ()) (Const Unique) m a
  } deriving
  ( Functor, Applicative, Monad, MonadFix
  , MonadSample t, MonadHold t, PostBuild t
  )

runUniqueSupplyT
  :: (Reflex t, MonadHold t m, MonadFix m)
  => Supply
  -> UniqueSupplyT t m a
  -> m a
runUniqueSupplyT initialSupply (UniqueSupplyT m) = mdo
  bSupply <- hold initialSupply $ snd <$> eResponse
  (a, eRequest) <- runRequesterT m $ fst <$> eResponse
  let eResponse = getUniques <$> bSupply <@> eRequest
  pure a
  where
    getUniques is =
      flip runState is .
      traverseRequesterData
        (\_ -> do
            (u, s') <- gets freshId
            put s'
            pure $ Const $ Unique u)

instance RandomGen t m => RandomGen t (UniqueSupplyT t m)
instance NotReady t m => NotReady t (UniqueSupplyT t m)
instance (Reflex t, Monad m) => UniqueSupply t (UniqueSupplyT t m) where
  requestUnique = UniqueSupplyT . fmap (fmap getConst) . requesting . (Const () <$)

instance MonadState s m => MonadState s (UniqueSupplyT t m) where
  get = lift get
  put = lift . put


instance MonadTrans (UniqueSupplyT t) where
  lift = UniqueSupplyT . lift

instance (Adjustable t m, MonadHold t m, MonadFix m) => Adjustable t (UniqueSupplyT t m) where
  runWithReplace a b = UniqueSupplyT $ runWithReplace (unUniqueSupplyT a) (unUniqueSupplyT <$> b)
  traverseIntMapWithKeyWithAdjust a b c =
    UniqueSupplyT $
    traverseIntMapWithKeyWithAdjust
      (\x y -> unUniqueSupplyT $ a x y)
      b
      c
  traverseDMapWithKeyWithAdjust a b c =
    UniqueSupplyT $
    traverseDMapWithKeyWithAdjust
      (\x y -> unUniqueSupplyT $ a x y)
      b
      c
  traverseDMapWithKeyWithAdjustWithMove a b c =
    UniqueSupplyT $
    traverseDMapWithKeyWithAdjustWithMove
      (\x y -> unUniqueSupplyT $ a x y)
      b
      c
