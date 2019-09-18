module Config where

import qualified Debug.Trace as D

data DebugLevel = DebugNone | DebugAll
  deriving (Eq, Ord, Show)

debugLevel = DebugNone

trace s a | debugLevel > DebugNone = D.trace s a
          | otherwise = a

traceM s | debugLevel > DebugNone = D.traceM s
         | otherwise = pure ()
