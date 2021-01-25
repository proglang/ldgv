module Parsing where

import Grammar
import Syntax
import Tokens

parse :: String -> [Decl]
parse = parseCalc . alexScanTokens
