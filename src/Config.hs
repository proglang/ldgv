module Config where

import qualified Debug.Trace as D
import qualified System.Exit as E
import PrettySyntax

data DebugLevel = DebugNone | DebugAll
  deriving (Eq, Ord, Show)

debugLevel = DebugNone
--debugLevel = DebugAll

trace s a | debugLevel > DebugNone = D.trace s a
          | otherwise = a

traceM s | debugLevel > DebugNone = D.traceM s
         | otherwise = pure ()

traceIO s | debugLevel > DebugNone = D.traceIO s
         | otherwise = pure ()


-- | helper for prettyprinting results depending on debug level
printResult :: (Pretty a, Pretty b) => (Either a b, s) -> IO ()
printResult (Left a, _) | debugLevel > DebugNone = putStrLn ("Error: " ++ pshow a)
                        | otherwise = E.die ("Error: " ++ pshow a)
printResult (Right b, _) | debugLevel > DebugNone = putStrLn ("Success: " ++ pshow b)
                         | otherwise = return ()

printDebug s | debugLevel > DebugNone = print s
printDebug s | otherwise = return ()

putDebugStr s | debugLevel > DebugNone = putStrLn s
putDebugStr s | otherwise = return ()
