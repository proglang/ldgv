module Networking.NetworkingMethod.NetworkingMethodCommon where

import GHC.IO.Handle
import qualified Control.Concurrent.Chan as Chan
import qualified Control.Concurrent.MVar as MVar
import qualified Data.Map as Map
import Networking.Messages
import qualified Control.Concurrent.SSem as SSem

-- type ActiveConnections = ActiveConnectionsStateless

type ActiveConnections = ActiveConnectionsFast

data ActiveConnectionsStateless = ActiveConnectionsStateless

type Connection = (Handle, MVar.MVar Bool, Chan.Chan (String, (String, Messages)), MVar.MVar (Map.Map String (String, Responses)), SSem.SSem)
--                             isClosed                Conversationid serial deserial
type ActiveConnectionsFast = MVar.MVar (Map.Map NetworkAddress Connection)

type NetworkAddress = (String, String)