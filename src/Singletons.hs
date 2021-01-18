{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilyDependencies #-}
module Singletons where

import Data.Kind

type family The k = (s :: k -> Type) | s -> k

class Known (x :: a) where sing :: The a x
