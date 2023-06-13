{-# LANGUAGE LambdaCase #-}

module ProcessEnvironment where
import ProcessEnvironmentTypes
import Control.Concurrent.MVar as MVar
import Control.Monad.Reader as T
import Data.Map as Map

import qualified Networking.NetworkingMethod.NetworkingMethodCommon as NMC

type InterpretM a = T.ReaderT (PEnv, (MVar.MVar (Map.Map Int ServerSocket), VChanConnections, NMC.ActiveConnections)) IO a