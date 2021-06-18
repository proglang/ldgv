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
import System.IO
import System.Process.Typed
import qualified Control.Monad.Fail as Fail

data Env = Env
  { envCC :: !String
  , envFlags :: ![String]
  , envVerbose :: !Bool
  }
  deriving stock (Show)

defaultEnv :: Env
defaultEnv = Env
  { envCC = "cc"
  , envFlags = []
  , envVerbose = False
  }

compile :: (MonadReader Env m, MonadIO m) => FilePath -> FilePath -> m ()
compile result src = do
  cc <- asks envCC
  defaultFlags <- liftIO cflags
  customFlags <- asks envFlags
  let args = "-o" : result : "-c" : src : defaultFlags ++ customFlags
  run cc args

link :: (MonadReader Env m, MonadIO m, Fail.MonadFail m) => FilePath -> [FilePath] -> String -> m ()
link result srcs backend = do
  cc <- asks envCC
  defaultFlagsCC <- liftIO cflags
  defaultFlagsLD <- liftIO ldflags
  customFlags <- asks envFlags
  backendSrcs <-
    if | pathSeparator `elem` backend ->
           pure [backend]
       | Just additional <- lookup backend knownBackends -> liftIO $
           traverse backendFile $ backendImplFileName backend : additional
       | otherwise -> Fail.fail $
           "unknown backend ›" ++ backend ++ "‹, use ›./" ++ backend ++ "‹ to refer to a file"

  let args = "-o" : result : concat
        [ srcs
        , defaultFlagsCC
        , defaultFlagsLD
        , backendSrcs
        , customFlags
        ]
  run cc args

run :: (MonadReader Env m, MonadIO m) => String -> [String] -> m ()
run exe args = do
  verbose <- asks envVerbose
  when verbose $ liftIO do
    hPutStrLn stderr $ unwords $ fmap show $ exe:args
  runProcess_ $ proc exe args

basePath :: FilePath
basePath = "c-support/runtime"

cflags :: IO [String]
cflags = do
  dataDir <- getDataDir
  pure ["-pthread", "-Wall", "-O2", "-fomit-frame-pointer", "-std=c11", "-I" ++ dataDir </> basePath ]

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
