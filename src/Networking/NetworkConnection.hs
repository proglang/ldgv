module Networking.NetworkConnection where

import Networking.DirectionalConnection
import qualified Control.Concurrent.MVar as MVar

data NetworkConnection a = NetworkConnection {ncRead :: DirectionalConnection a, ncWrite :: DirectionalConnection a, ncPartnerUserID :: Maybe String, ncOwnUserID :: Maybe String, ncConnectionState :: MVar.MVar ConnectionState}
    deriving Eq

data ConnectionState = Connected {csHostname :: String, csPort :: String}
                     | Disconnected
                     | Emulated
    deriving Eq


newNetworkConnection :: String -> String -> String -> String -> IO (NetworkConnection a)
newNetworkConnection partnerID ownID hostname port = do
    read <- newConnection
    write <- newConnection
    connectionstate <- MVar.newEmptyMVar 
    MVar.putMVar connectionstate $ Connected hostname port
    return $ NetworkConnection read write (Just partnerID) (Just ownID) connectionstate