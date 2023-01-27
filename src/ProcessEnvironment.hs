{-# LANGUAGE LambdaCase #-}

module ProcessEnvironment where
import ProcessEnvironmentTypes
import Syntax as S
import GHC.IO.Handle
import Control.Concurrent.Chan as C
import Control.Concurrent.MVar as MVar
import Control.Monad.Reader as T
import Data.Set (Set)
import Data.Map as Map
import qualified Data.Set as Set
import Kinds (Multiplicity(..))

import qualified Data.Maybe

import Networking.DirectionalConnection
import qualified Networking.NetworkConnection as NCon
-- import qualified Networking.Common as NC

import Network.Socket
import qualified Networking.NetworkConnection as NCon
import qualified Networking.NetworkConnection as NCOn
import qualified Networking.NetworkConnection as Ncon

import qualified Networking.NetworkingMethod.NetworkingMethodCommon as NMC
-- import qualified Networking.Common as NC

-- | the interpretation monad
-- type InterpretM a = T.ReaderT (PEnv, (MVar.MVar (Map.Map Int ServerSocket), MVar.MVar ActiveConnections)) IO a
type InterpretM a = T.ReaderT (PEnv, (MVar.MVar (Map.Map Int ServerSocket), NMC.ActiveConnections)) IO a