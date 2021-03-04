module Parsing where

import Parsing.Grammar as G
import Parsing.Tokens
import Syntax

parseDecls :: String -> [Decl]
parseDecls = G.parseDecls . alexScanTokens

parseType :: String -> Type
parseType = G.parseType . alexScanTokens
