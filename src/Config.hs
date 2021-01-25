{-# LANGUAGE OverloadedStrings #-}
module Config where

import qualified Debug.Trace as D
import PrettySyntax (Pretty, pshow)
import MonadOut (MonadOut(..))

data DebugLevel = DebugNone | DebugAll
  deriving (Eq, Ord, Show)

--debugLevel = DebugAll
debugLevel = DebugNone

trace s a | debugLevel > DebugNone = D.trace s a
          | otherwise = a

traceM s | debugLevel > DebugNone = D.traceM s
         | otherwise = pure ()

traceIO s | debugLevel > DebugNone = D.traceIO s
          | otherwise = pure ()

printSuccess :: (Pretty a, MonadOut m) => a -> m ()
printSuccess a
  | debugLevel > DebugNone = output $ "Success: " ++ pshow a
  | otherwise = output "Success"

printDebug s | debugLevel > DebugNone = output $ show s
printDebug s | otherwise = return ()
