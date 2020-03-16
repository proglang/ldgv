{-# LANGUAGE OverloadedStrings #-}
module Config where

import qualified Debug.Trace as D
import qualified Data.Text as T
import Language.Javascript.JSaddle
import PrettySyntax (Pretty, pshow)

data DebugLevel = DebugNone | DebugAll
  deriving (Eq, Ord, Show)

debugLevel = DebugAll

trace s a | debugLevel > DebugNone = D.trace s a
          | otherwise = a

traceM s | debugLevel > DebugNone = D.traceM s
         | otherwise = pure ()

traceIO s | debugLevel > DebugNone = D.traceIO s
          | otherwise = pure ()

-- append text to our result textarea
printLn :: String -> JSM ()
printLn s = do
    let t' = T.replace "\n" "\\n" $ T.pack s
    let t = T.replace "'" "\\'" t'
    val <- eval $ T.concat ["document.getElementById('tOutput').value += '", t, "\\n'"]
    pure ()

-- | helper for prettyprinting results depending on debug level
printResult :: (Pretty a, Pretty b) => (Either a b, s) -> IO ()
--printResult (Left a, _) | debugLevel > DebugNone = printLn ("Error: " ++ pshow a)
--                        | otherwise = fail ("Error: " ++ pshow a)
printResult (Left a, _) = fail ("Error: " ++ pshow a)
printResult (Right b, _) | debugLevel > DebugNone = printLn ("Success: " ++ pshow b)
                         | otherwise = return ()

printDebug s | debugLevel > DebugNone = printLn $ show s
printDebug s | otherwise = return ()

putStrLn = printLn
