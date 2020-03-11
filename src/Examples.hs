{-# LANGUAGE TemplateHaskell #-}
module Examples where

import Data.Map (fromList)
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8)
import Data.List (isSuffixOf)
import Data.FileEmbed

_examples = filter (\(name, _) -> isSuffixOf ".ldgv" name) $(embedDir "examples")
examples = fromList $ map (\(name, content) -> (name, unpack $ decodeUtf8 content)) _examples 

filenames = map fst _examples
