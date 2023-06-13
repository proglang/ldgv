module Networking.NetworkingMethod.NetworkingMethodCommon where

import GHC.IO.Handle
import Network.Socket
import Networking.Messages
import qualified Control.Concurrent.Chan as Chan
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.SSem as SSem
import qualified Data.Map as Map

-- type ActiveConnections = ActiveConnectionsStateless

type ActiveConnections = ActiveConnectionsFast

data ActiveConnectionsStateless = ActiveConnectionsStateless

type ConversationStateless = (Handle, (Socket, SockAddr))

type Connection = (ConversationStateless, MVar.MVar Bool, Chan.Chan (String, (String, Message)), MVar.MVar (Map.Map String (String, Response)), SSem.SSem)
--                                            isClosed                Conversationid serial deserial
type ActiveConnectionsFast = MVar.MVar (Map.Map NetworkAddress Connection)

type NetworkAddress = (String, String)
