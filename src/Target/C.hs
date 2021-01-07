{-# OPTIONS_GHC -Wall #-}
module Target.C where

import Data.ByteString.Builder
import Syntax

generate :: [Decl] -> Builder
generate _ = error "generate: not implemented"
