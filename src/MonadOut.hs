{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module MonadOut where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Fail

-- Can we loosen the requirement to ›Monad‹?
class MonadFail m => MonadOut m where
  output :: String -> m ()

newtype OutIOT m a = OutIO { runOutIO :: m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadFail)

instance (MonadIO m, MonadFail m) => MonadOut (OutIOT m) where
  output = liftIO . putStrLn

instance MonadTrans OutIOT where
  lift = OutIO
