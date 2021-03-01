{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
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

link :: (MonadReader Env m, MonadIO m, MonadFail m) => FilePath -> [FilePath] -> String -> m ()
link result srcs backend = do
  ld <- asks envLD
  defaultFlagsCC <- liftIO cflags
  defaultFlagsLD <- liftIO ldflags
  customFlagsCC <- asks envCCFlags
  customFlagsLD <- asks envLDFlags
  backendSrcs <-
    if | any (== pathSeparator) backend ->
           pure [backend]
       | Just additional <- lookup backend knownBackends -> liftIO $
           traverse backendFile $ backendImplFileName backend : additional
       | otherwise -> fail $
           "unknown backend ›" ++ backend ++ "‹, use ›./" ++ backend ++ "‹ to refer to a file"

  let args = "-o" : result : concat
        [ srcs
        , defaultFlagsCC
        , defaultFlagsLD
        , backendSrcs
        , customFlagsCC
        , customFlagsLD
        ]
  runProcess_ $ proc ld args

basePath :: FilePath
basePath = "c-support/runtime"

cflags :: IO [String]
cflags = do
  dataDir <- getDataDir
  pure ["-O2", "-fomit-frame-pointer", "-std=c11", "-I" ++ dataDir </> basePath ]

ldflags :: IO [String]
ldflags = do
  supportImpl <- backendFile "LDST.c"
  pure [supportImpl]

listBackends :: IO [(String, FilePath)]
listBackends = do
  for knownBackends \(b, _) -> (b,) <$> backendFile (backendImplFileName b)

knownBackends :: [(String, [FilePath])]
knownBackends =
  [ ("serial", [])
  , ("concurrent", ["thpool.c"])
  ]

backendImplFileName :: String -> FilePath
backendImplFileName b = "LDST_" ++ b <.> "c"

backendFile :: String -> IO FilePath
backendFile filename = getDataFileName $ basePath </> filename
