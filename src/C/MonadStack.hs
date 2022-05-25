{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A monad transformer adding a stack, allowing for 'pushStack' and
-- 'popStack' operations.
--
-- For now this module provides only the instances used in other parts of the
-- code.
module C.MonadStack (StackT, evalStack, evalStackT, hoistStack, generalizeStack, MonadStack(..)) where

import Control.Monad.Error.Class
import Control.Monad.State.Strict
import Control.Monad.Writer.Class
import Control.Monad.RWS.Strict
import Control.Monad.Writer.Strict
import Data.Functor.Identity

newtype StackT s m a = StackT { unStackT :: StateT [s] m a }
  deriving newtype (Functor, Applicative, Monad, MonadError e, MonadWriter w, MonadTrans)

evalStackT :: Monad m => [s] -> StackT s m a -> m a
evalStackT ss = flip evalStateT ss . unStackT

evalStack :: [s] -> StackT s Identity a -> a
evalStack ss = runIdentity . evalStackT ss

class Monad m => MonadStack s m | m -> s where
  pushStack :: s -> m ()
  popStack :: m (Maybe s)

instance Monad m => MonadStack s (StackT s m) where
  pushStack s = StackT $ modify (s :)
  popStack = StackT do
    ss <- get
    case ss of
      [] -> pure Nothing
      s:ss' -> Just s <$ put ss'

instance (MonadStack s m, Monoid w) => MonadStack s (RWST r w s' m) where
  pushStack = lift . pushStack
  popStack = lift popStack

instance (MonadStack s m, Monoid w) => MonadStack s (WriterT w m) where
  pushStack = lift . pushStack
  popStack = lift popStack

hoistStack :: (forall x. m x -> n x) -> StackT s m a -> StackT s n a
hoistStack f = StackT . mapStateT f . unStackT

generalizeStack :: Applicative m => StackT s Identity a -> StackT s m a
generalizeStack = hoistStack (pure . runIdentity)
