module Parsing where

import Grammar
import Syntax
import Tokens
import MonadOut (MonadOut(..))

parse :: String -> [Decl]
parse = parseCalc . alexScanTokens
