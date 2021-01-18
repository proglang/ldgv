{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module MonadOut where

import Control.Monad.Fail
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

-- Can we loosen the requirement to ›Monad‹?
class MonadFail m => MonadOut m where
  output :: String -> m ()

newtype OutIOT m a = OutIO { runOutIO :: m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadFail)

newtype OutIgnore m a = OutIgnore { runOutIgnore :: m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadFail)

instance (MonadIO m, MonadFail m) => MonadOut (OutIOT m) where
  output = liftIO . putStrLn

instance MonadTrans OutIOT where
  lift = OutIO

instance (MonadFail m) => MonadOut (OutIgnore m) where
  output _ = pure ()

instance MonadTrans OutIgnore where
  lift = OutIgnore
