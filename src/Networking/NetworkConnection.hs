module Networking.NetworkConnection where

import Networking.DirectionalConnection
import qualified Data.Maybe
import qualified Control.Concurrent.MVar as MVar

data NetworkConnection a = NetworkConnection {ncRead :: DirectionalConnection a, ncWrite :: DirectionalConnection a, ncPartnerUserID :: Maybe String, ncOwnUserID :: Maybe String, ncConnectionState :: MVar.MVar ConnectionState}
    deriving Eq

data ConnectionState = Connected {csHostname :: String, csPort :: String}
                     | Disconnected
                     | Emulated
                     | RedirectRequest {csHostname :: String, csPort :: String} -- Asks to redirect to this connection
    deriving Eq


newNetworkConnection :: String -> String -> String -> String -> IO (NetworkConnection a)
newNetworkConnection partnerID ownID hostname port = do
    read <- newConnection
    write <- newConnection
    connectionstate <- MVar.newEmptyMVar 
    MVar.putMVar connectionstate $ Connected hostname port
    return $ NetworkConnection read write (Just partnerID) (Just ownID) connectionstate


createNetworkConnection :: [a] -> Int -> [a] -> Int -> Maybe String -> Maybe String -> String -> String -> IO (NetworkConnection a)
createNetworkConnection readList readNew writeList writeNew partnerID ownID hostname port = do
    read <- createConnection readList readNew
    write <- createConnection writeList writeNew
    connectionstate <- MVar.newEmptyMVar
    MVar.putMVar connectionstate $ Connected hostname port
    return $ NetworkConnection read write partnerID ownID connectionstate


createNetworkConnectionS :: ([a], Int) -> ([a], Int) -> String -> String -> (String, String) -> IO (NetworkConnection a)
createNetworkConnectionS (readList, readNew) (writeList, writeNew) partnerID ownID (hostname, port) = createNetworkConnection readList readNew writeList writeNew (Just partnerID) (Just ownID) hostname port


newEmulatedConnection :: DirectionalConnection a -> DirectionalConnection a -> IO (NetworkConnection a)
newEmulatedConnection r w = do
    connectionstate <- MVar.newEmptyMVar 
    MVar.putMVar connectionstate Emulated
    return $ NetworkConnection r w Nothing Nothing connectionstate

serializeNetworkConnection :: NetworkConnection a -> IO ([a], Int, [a], Int, String, String, String, String)
serializeNetworkConnection nc = do
    constate <- MVar.readMVar $ ncConnectionState nc
    (readList, readUnread) <- serializeConnection $ ncRead nc
    (writeList, writeUnread) <- serializeConnection $ ncWrite nc
    (address, port) <- case constate of
        Connected address port -> return (address, port)
        RedirectRequest address port -> return (address, port)
        _ -> return ("", "")
    return (readList, readUnread, writeList, writeUnread, Data.Maybe.fromMaybe "" $ ncPartnerUserID nc, Data.Maybe.fromMaybe "" $ ncOwnUserID nc, address, port)