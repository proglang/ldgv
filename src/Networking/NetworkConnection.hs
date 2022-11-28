module Networking.NetworkConnection where

{-
import Networking.DirectionalConnection
import GHC.IO.Handle
import Network.Run.TCP


data NetworkingConnection a = NetworkingConnection {ingoing :: DirectionalConnection a, outgoing :: DirectionalConnection a, networkHandle :: Handle}

type Hostname = String
type Port = Int

newConnection :: Maybe Hostname -> Port -> NetworkingConnection a
newConnection maybeHost port = do
    case maybeHost of
        Nothing -> runTCPServer Nothing (show port) (NC.communicate r w)
        -}
