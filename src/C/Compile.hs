{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall #-}

module C.Compile
  ( Env(..)
  , defaultEnv
  , compile
  , link
  , cflags
  , listBackends
  ) where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Traversable
import Paths_ldgv
import System.FilePath
import System.Process.Typed

data Env = Env
  { envCC :: String
  , envLD :: String
  , envCCFlags :: [String]
  , envLDFlags :: [String]
  }
  deriving stock (Show)

defaultEnv :: Env
defaultEnv = Env
  { envCC = "cc"
  , envLD = "cc"
  , envCCFlags = []
  , envLDFlags = []
  }

compile :: (MonadReader Env m, MonadIO m) => FilePath -> FilePath -> m ()
compile result src = do
  cc <- asks envCC
  defaultFlags <- liftIO cflags
  customFlags <- asks envCCFlags
  let args = "-o" : result : "-c" : src : defaultFlags ++ customFlags
  runProcess_ $ proc cc args

link :: (MonadReader Env m, MonadIO m) => FilePath -> [FilePath] -> m ()
link result srcs = do
  ld <- asks envLD
  defaultFlags <- liftIO cflags
  customFlagsA <- asks envCCFlags
  customFlagsB <- asks envLDFlags
  let args = "-o" : result : srcs ++ defaultFlags ++ customFlagsA ++ customFlagsB
  runProcess_ $ proc ld args

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
