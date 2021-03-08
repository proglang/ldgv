{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall #-}

module Parsing where

import Parsing.Grammar as G
import Parsing.Tokens
import Syntax

parseDecls :: String -> Either String [Decl]
parseDecls = flip runAlex G.parseDecls

parseType :: String -> Either String Type
parseType = flip runAlex G.parseType
