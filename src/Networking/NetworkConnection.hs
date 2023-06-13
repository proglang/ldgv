{-# LANGUAGE BlockArguments #-}
module Networking.NetworkConnection where

import Networking.NetworkBuffer
import Networking.RandomID
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.SSem as SSem
import qualified Data.Map as Map

data NetworkConnection a = NetworkConnection {ncRead :: NetworkBuffer a, ncWrite :: NetworkBuffer a, ncPartnerUserID :: String, ncOwnUserID :: String, ncConnectionState :: MVar.MVar ConnectionState, ncHandlingIncomingMessage :: SSem.SSem, ncReadyToBeUsed :: MVar.MVar Bool}
    deriving Eq

data ConnectionState = Connected {csHostname :: String, csPort :: String, csPartnerConnectionID :: String, csOwnConnectionID :: String, csConfirmedConnection :: Bool}
                     | Disconnected {csHostname :: String, csPort :: String, csPartnerConnectionID :: String, csOwnConnectionID :: String, csConfirmedConnection :: Bool}
                     | Emulated {csPartnerConnectionID :: String, csOwnConnectionID :: String, csConfirmedConnection :: Bool}
                     | RedirectRequest {csHostname :: String, csPort :: String, csRedirectHostname :: String, csRedirectPort :: String, csPartnerConnectionID :: String, csOwnConnectionID :: String, csConfirmedConnection :: Bool} -- Asks to redirect to this connection
    deriving (Eq, Show)


newNetworkConnection :: String -> String -> String -> String -> String -> String -> IO (NetworkConnection a)
newNetworkConnection partnerID ownID hostname port partnerConnectionID ownConnectionID = do
    read <- newNetworkBuffer 
    write <- newNetworkBuffer 
    connectionstate <- MVar.newMVar $ Connected hostname port partnerConnectionID ownConnectionID True
    incomingMsg <- SSem.new 1
    readyForUse <- MVar.newMVar True
    return $ NetworkConnection read write partnerID ownID connectionstate incomingMsg readyForUse


createNetworkConnection :: ([a], Int, Int) -> ([a], Int, Int) -> String -> String -> (String, String, String) -> IO (NetworkConnection a)
createNetworkConnection (readList, readOffset, readLength) (writeList, writeOffset, writeLength) partnerID ownID (hostname, port, partnerConnectionID) = do
    read <- deserializeMinimal (readList, readOffset, readLength)
    write <- deserializeMinimal (writeList, writeOffset, writeLength)
    ownConnectionID <- newRandomID
    connectionstate <- if port=="" then 
            MVar.newMVar $ Disconnected "" "" partnerConnectionID ownConnectionID True 
        else 
            MVar.newMVar $ Connected hostname port partnerConnectionID ownConnectionID False
    incomingMsg <- SSem.new 1
    readyForUse <- MVar.newMVar True
    return $ NetworkConnection read write partnerID ownID connectionstate incomingMsg readyForUse

setReadyForUse :: NetworkConnection a -> Bool -> IO ()
setReadyForUse nc ready = do 
    old <- MVar.takeMVar (ncReadyToBeUsed nc)
    MVar.putMVar (ncReadyToBeUsed nc) ready
    -- putStrLn $ "setReadyForUse for: " ++ ncOwnUserID nc ++ " was: " ++ show old ++ " now is: " ++ show ready


isReadyForUse :: NetworkConnection a -> IO Bool
isReadyForUse nc = MVar.readMVar $ ncReadyToBeUsed nc

newEmulatedConnection :: MVar.MVar (Map.Map String (NetworkConnection a)) -> IO (NetworkConnection a, NetworkConnection a)
newEmulatedConnection mvar = do
    ncmap <- MVar.takeMVar mvar
    read <- newNetworkBuffer 
    write <- newNetworkBuffer
    read2 <- newNetworkBuffer
    write2 <- newNetworkBuffer
    connectionid1 <- newRandomID 
    connectionid2 <- newRandomID 
    connectionstate <- MVar.newMVar $ Emulated connectionid2 connectionid1 True
    connectionstate2 <- MVar.newMVar $ Emulated connectionid1 connectionid2 True
    userid <- newRandomID
    userid2 <- newRandomID
    incomingMsg <- SSem.new 1
    incomingMsg2 <- SSem.new 1
    readyForUse1 <- MVar.newMVar True
    readyForUse2 <- MVar.newMVar True 
    let nc1 = NetworkConnection read write userid2 userid connectionstate incomingMsg readyForUse1
    let nc2 = NetworkConnection read2 write2 userid userid2 connectionstate2 incomingMsg2 readyForUse2
    let ncmap1 = Map.insert userid2 nc1 ncmap
    let ncmap2 = Map.insert userid nc2 ncmap1
    MVar.putMVar mvar ncmap2
    return (nc1, nc2)

serializeNetworkConnection :: NetworkConnection a -> IO ([a], Int, Int, [a], Int, Int, String, String, String, String, String)
serializeNetworkConnection nc = do
    constate <- MVar.readMVar $ ncConnectionState nc
    (readList, readOffset, readLength) <- serializeMinimal $ ncRead nc
    (writeList, writeOffset, writeLength) <- serializeMinimal $ ncWrite nc
    (address, port, partnerConnectionID) <- case constate of
        Connected address port partnerConnectionID _ _ -> return (address, port, partnerConnectionID)
        RedirectRequest address port _ _ partnerConnectionID _ _ -> return (address, port, partnerConnectionID)
        _ -> return ("", "", csPartnerConnectionID constate)
    return (readList, readOffset, readLength, writeList, writeOffset, writeLength, ncPartnerUserID nc, ncOwnUserID nc, address, port, partnerConnectionID)

changePartnerAddress :: NetworkConnection a -> String -> String -> String -> IO ()
changePartnerAddress con hostname port partnerConnectionID = do
    oldConnectionState <- MVar.takeMVar $ ncConnectionState con
    MVar.putMVar (ncConnectionState con) $ Connected hostname port partnerConnectionID (csOwnConnectionID oldConnectionState) $ csConfirmedConnection oldConnectionState

disconnectFromPartner :: NetworkConnection a -> IO ()
disconnectFromPartner con = do
    oldConnectionState <- MVar.takeMVar $ ncConnectionState con
    case oldConnectionState of
        Emulated {} -> 
            MVar.putMVar (ncConnectionState con) $ Disconnected "" "" (csPartnerConnectionID oldConnectionState) (csOwnConnectionID oldConnectionState) True
        _ -> MVar.putMVar (ncConnectionState con) $ Disconnected (csHostname oldConnectionState) (csPort oldConnectionState) (csPartnerConnectionID oldConnectionState) (csOwnConnectionID oldConnectionState) True

isConnectionConfirmed :: NetworkConnection a -> IO Bool
isConnectionConfirmed con = do
    conState <- MVar.readMVar $ ncConnectionState con
    return $ csConfirmedConnection conState

confirmConnectionID :: NetworkConnection a -> String -> IO Bool
confirmConnectionID con ownConnectionID = do
    conState <- MVar.takeMVar $ ncConnectionState con
    if ownConnectionID == csOwnConnectionID conState then do
        newConState <- case conState of
            Connected host port part own conf -> return $ Connected host port part own True
            Disconnected host port part own conf -> return $ Disconnected host port part own True
            Emulated part own conf -> return $ Emulated part own True
            RedirectRequest host port rehost report part own conf -> return $ RedirectRequest host port rehost report part own True
        MVar.putMVar (ncConnectionState con) newConState
        return True
        else do 
            MVar.putMVar (ncConnectionState con) conState
            return False

tryConvertToEmulatedConnection  :: MVar.MVar (Map.Map String (NetworkConnection a)) -> NetworkConnection a -> IO Bool
tryConvertToEmulatedConnection vchans con = do     
    -- networkConnectionsMap <- MVar.takeMVar vchans
    networkConnectionsMap <- MVar.readMVar vchans
    case Map.lookup (ncOwnUserID con) networkConnectionsMap of
        Just partner -> SSem.withSem (ncHandlingIncomingMessage partner) do
            connectionState <- MVar.takeMVar $ ncConnectionState partner
            case connectionState of
                Connected {} -> do
                    partConID <- newRandomID
                    ownConID <- newRandomID
                    MVar.putMVar (ncConnectionState partner) $ Emulated ownConID partConID True
                    MVar.takeMVar $ ncConnectionState con
                    MVar.putMVar (ncConnectionState con) $ Emulated partConID ownConID True
                    -- MVar.putMVar vchans networkConnectionsMap
                    return True
                _ -> do 
                    -- MVar.putMVar vchans networkConnectionsMap
                    return False
        _ -> do 
            -- MVar.putMVar vchans networkConnectionsMap
            return False

