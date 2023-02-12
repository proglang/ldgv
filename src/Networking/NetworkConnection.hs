module Networking.NetworkConnection where

import Networking.DirectionalConnection
import Networking.UserID
import qualified Data.Maybe
import qualified Data.Map as Map
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.SSem as SSem

data NetworkConnection a = NetworkConnection {ncRead :: DirectionalConnection a, ncWrite :: DirectionalConnection a, ncPartnerUserID :: Maybe String, ncOwnUserID :: Maybe String, ncConnectionState :: MVar.MVar ConnectionState, ncHandlingIncomingMessage :: SSem.SSem}
                         | NetworkConnectionPlaceholder {ncPartnerUserID :: Maybe String, ncConnectionState :: MVar.MVar ConnectionState, ncHandlingIncomingMessage :: SSem.SSem}
    deriving Eq

data ConnectionState = Connected {csHostname :: String, csPort :: String}
                     | Disconnected
                     | Emulated
                     | RedirectRequest {csHostname :: String, csPort :: String, csRedirectHostname :: String, csRedirectPort :: String} -- Asks to redirect to this connection
    deriving (Eq, Show)

newPlaceHolderConnection :: String -> String -> String -> IO (NetworkConnection a)
newPlaceHolderConnection partnerID hostname port = do
    connectionstate <- MVar.newMVar $ Connected hostname port
    incomingMsg <- SSem.new 1
    return $ NetworkConnectionPlaceholder (Just partnerID) connectionstate incomingMsg


newNetworkConnection :: String -> String -> String -> String -> IO (NetworkConnection a)
newNetworkConnection partnerID ownID hostname port = do
    read <- newConnection
    write <- newConnection
    connectionstate <- MVar.newMVar $ Connected hostname port
    incomingMsg <- SSem.new 1
    return $ NetworkConnection read write (Just partnerID) (Just ownID) connectionstate incomingMsg

newNetworkConnectionAllowingMaybe :: Maybe String -> Maybe String -> String -> String -> IO (NetworkConnection a)
newNetworkConnectionAllowingMaybe partnerID ownID hostname port = do
    read <- newConnection
    write <- newConnection
    connectionstate <- MVar.newMVar $ Connected hostname port
    incomingMsg <- SSem.new 1
    return $ NetworkConnection read write partnerID ownID connectionstate incomingMsg


createNetworkConnection :: [a] -> Int -> [a] -> Int -> Maybe String -> Maybe String -> String -> String -> IO (NetworkConnection a)
createNetworkConnection readList readNew writeList writeNew partnerID ownID hostname port = do
    read <- createConnection readList readNew
    write <- createConnection writeList writeNew
    connectionstate <- MVar.newMVar $ Connected hostname port
    incomingMsg <- SSem.new 1
    return $ NetworkConnection read write partnerID ownID connectionstate incomingMsg


createNetworkConnectionS :: ([a], Int) -> ([a], Int) -> String -> String -> (String, String) -> IO (NetworkConnection a)
createNetworkConnectionS (readList, readNew) (writeList, writeNew) partnerID ownID (hostname, port) = createNetworkConnection readList readNew writeList writeNew (Just partnerID) (Just ownID) hostname port


{-newEmulatedConnection :: DirectionalConnection a -> DirectionalConnection a -> IO (NetworkConnection a)
newEmulatedConnection r w = do
    connectionstate <- MVar.newEmptyMVar 
    MVar.putMVar connectionstate Emulated
    incomingMsg <- SSem.new 1
    return $ NetworkConnection r w Nothing Nothing connectionstate incomingMsg-}

newEmulatedConnection :: MVar.MVar (Map.Map String (NetworkConnection a)) -> IO (NetworkConnection a, NetworkConnection a)
newEmulatedConnection mvar = do
    ncmap <- MVar.takeMVar mvar
    read <- newConnection
    write <- newConnection
    read2 <- newConnection
    write2 <- newConnection
    connectionstate <- MVar.newMVar Emulated
    connectionstate2 <- MVar.newMVar Emulated
    userid <- newRandomUserID
    userid2 <- newRandomUserID
    incomingMsg <- SSem.new 1
    incomingMsg2 <- SSem.new 1
    let nc1 = NetworkConnection read write (Just userid2) (Just userid) connectionstate incomingMsg
    let nc2 = NetworkConnection read2 write2 (Just userid) (Just userid2) connectionstate2 incomingMsg2
    let ncmap1 = Map.insert userid2 nc1 ncmap
    let ncmap2 = Map.insert userid nc2 ncmap1
    MVar.putMVar mvar ncmap2
    return (nc1, nc2)



serializeNetworkConnection :: NetworkConnection a -> IO ([a], Int, [a], Int, String, String, String, String)
serializeNetworkConnection nc = do
    constate <- MVar.readMVar $ ncConnectionState nc
    (readList, readUnread) <- serializeConnection $ ncRead nc
    (writeList, writeUnread) <- serializeConnection $ ncWrite nc
    (address, port) <- case constate of
        Connected address port -> return (address, port)
        RedirectRequest address port _ _-> return (address, port)
        _ -> return ("", "")
    return (readList, readUnread, writeList, writeUnread, Data.Maybe.fromMaybe "" $ ncPartnerUserID nc, Data.Maybe.fromMaybe "" $ ncOwnUserID nc, address, port)

changePartnerAddress :: NetworkConnection a -> String -> String -> IO ()
changePartnerAddress con hostname port = do
    _ <- MVar.takeMVar $ ncConnectionState con
    MVar.putMVar (ncConnectionState con) $ Connected hostname port