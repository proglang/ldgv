{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall #-}

module Parsing where

import Parsing.Grammar as G
import Parsing.Tokens
import Syntax

scanTokens :: String -> Either String [T]
scanTokens inp = runAlex inp go
  where
    go = do
      mt <- alexMonadScan
      case mt of
        Nothing -> pure []
        Just t -> (t:) <$> go

parseDecls :: String -> Either String [Decl]
parseDecls = fmap G.parseDecls . scanTokens

parseType :: String -> Either String Type
parseType = fmap G.parseType . scanTokens
