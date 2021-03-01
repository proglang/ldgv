{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall #-}

module C.Compile (listBackends, cflags) where

import Data.Traversable
import Paths_ldgv
import System.FilePath

basePath :: FilePath
basePath = "c-support/runtime"

cflags :: IO [String]
cflags = do
  dataDir <- getDataDir
  pure ["-O2", "-fomit-frame-pointer", "-std=c11", "-I" ++ dataDir </> basePath ]

listBackends :: IO [(String, FilePath)]
listBackends = do
  let backendFileName b = basePath </> "LDST_" ++ b ++ ".c"
  for knownBackends \(b, _) -> (b,) <$> getDataFileName (backendFileName b)

knownBackends :: [(String, [FilePath])]
knownBackends =
  [ ("serial", [])
  , ("concurrent", ["thpool.c"])
  ]
