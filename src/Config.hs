{-# LANGUAGE OverloadedStrings #-}
module Config where

import qualified Debug.Trace as D
import Syntax.Pretty (Pretty, pshow)
import Control.Monad.IO.Class

data DebugLevel = DebugNone | DebugAll
  deriving (Eq, Ord, Show)

--debugLevel = DebugAll
debugLevel = DebugNone

trace :: String -> a -> a
trace s a | debugLevel > DebugNone = D.trace s a
          | otherwise = a

traceM :: Applicative f => String -> f ()
traceM s | debugLevel > DebugNone = D.traceM s
         | otherwise = pure ()

traceShowM :: (Show a, Applicative f) => a -> f ()
traceShowM = traceM . show

traceIO :: MonadIO m => String -> m ()
traceIO s | debugLevel > DebugNone = liftIO $ D.traceIO s
          | otherwise = pure ()

traceSuccess :: (Pretty a, Applicative f) => a -> f ()
traceSuccess a
  | debugLevel > DebugNone = traceM $ "Success: " ++ pshow a
  | otherwise = traceM "Success"
