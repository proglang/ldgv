{-# LANGUAGE TemplateHaskell #-}
module Examples where

import Data.Map (Map, fromList)
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8)
import Data.List (isSuffixOf)
import Data.FileEmbed
import Data.ByteString (ByteString)

_examples :: [(String, ByteString)]
_examples = filter (\(name, _) -> ".ldgv" `isSuffixOf` name) $(embedDir "examples")

examples :: Map String String
examples = fromList $ map (\(name, content) -> (name, unpack $ decodeUtf8 content)) _examples

filenames :: [String]
filenames = map fst _examples
