{-# language DefaultSignatures, GADTs #-}
{-# language FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses #-}
{-# language UndecidableInstances #-}
module RandomGen.Class where

import Reflex
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Writer (WriterT)

class (Reflex t, Monad m) => RandomGen t m | m -> t where
  randomInt :: Event t x -> m (Event t Int)
  default randomInt
    :: (MonadTrans u, RandomGen t n, m ~ u n)
    => Event t x -> m (Event t Int)
  randomInt = lift . randomInt

  randomIntR :: Event t (Int, Int) -> m (Event t Int)
  default randomIntR
    :: (MonadTrans u, RandomGen t n, m ~ u n)
    => Event t (Int, Int) -> m (Event t Int)
  randomIntR = lift . randomIntR

randomPosition
  :: ( MonadHold t m, RandomGen t m
     , Num a, Num b
     )
  => Event t x
  -> (Int, Int) -- ^ x coordinate bounds
  -> (Int, Int) -- ^ y coordinate bounds
  -> m (Event t (a, b))
randomPosition eCreate xBounds yBounds = do
  eX <- fmap fromIntegral <$> randomIntR (xBounds <$ eCreate)
  eY <- fmap fromIntegral <$> randomIntR (yBounds <$ eCreate)
  switchHoldPromptly never ((\w -> (,) w <$> eY) <$> eX)

instance RandomGen t m => RandomGen t (StateT s m)
instance (RandomGen t m, Monoid w) => RandomGen t (WriterT w m)
instance RandomGen t m => RandomGen t (ReaderT e m)
instance RandomGen t m => RandomGen t (EventWriterT t w m)
