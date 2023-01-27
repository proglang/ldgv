module Networking.NetworkingMethod.NetworkingMethodCommon where

import GHC.IO.Handle
import qualified Control.Concurrent.Chan as Chan
import qualified Control.Concurrent.MVar as MVar
import qualified Data.Map as Map
import Networking.Messages

type ActiveConnections = ActiveConnectionsStateless

data ActiveConnectionsStateless = ActiveConnectionsStateless

type Connection = (Handle, Chan.Chan (String, (String, Messages)), MVar.MVar (Map.Map String (String, Responses)))

type ActiveConnectionsFast = MVar.MVar (Map.Map NetworkAddress Connection)

type NetworkAddress = (String, String)