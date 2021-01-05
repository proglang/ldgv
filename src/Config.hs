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

-- | helper for prettyprinting results depending on debug level
printResult :: MonadOut m => (Pretty a, Pretty b) => (Either a b, s) -> m ()
printResult (Left a, _) = fail ("Error: " ++ pshow a)
printResult (Right b, _) | debugLevel > DebugNone = printLn $ "Success: " ++ pshow b
                         | otherwise = printLn "Success"

printDebug s | debugLevel > DebugNone = printLn $ show s
printDebug s | otherwise = return ()

putStrLn s = printLn s
printLn s = output s

