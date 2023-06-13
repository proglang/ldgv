{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
module Networking.Incoming where

import Control.Concurrent
import Control.Monad
import Network.Socket
import Networking.Messages
import Networking.NetworkConnection
import ProcessEnvironmentTypes
import qualified Config
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.SSem as SSem
import qualified Data.Bifunctor
import qualified Data.Map as Map
import qualified Data.Maybe
import qualified Networking.Common as NC
import qualified Networking.Messages as Messages
import qualified Networking.NetworkBuffer as NB
import qualified Networking.NetworkConnection as NCon
import qualified Networking.NetworkingMethod.NetworkingMethodCommon as NMC
import qualified Networking.Outgoing as NO
import qualified Networking.RandomID as RandomID
import qualified Networking.Serialize as NSerialize
import qualified Syntax

handleClient :: NMC.ActiveConnections -> MVar.MVar (Map.Map String (NetworkConnection Value)) -> MVar.MVar [(String, (Syntax.Type, Syntax.Type))] -> (Socket, SockAddr) -> NC.ConversationOrHandle -> String -> String -> Message -> IO ()
handleClient activeCons mvar clientlist clientsocket hdl ownport message deserialmessages = do
    let userid = getUserID deserialmessages
    clientHostaddress <- case snd clientsocket of
        SockAddrInet _ hostname -> return $ hostaddressTypeToString hostname
        _ -> do
            receivedNetLog message "Error during receiving a networkmessage: only ipv4 is currently supported!"
            return ""

    netcons <- MVar.readMVar mvar
    case Map.lookup userid netcons of
        Just ncToPartner -> do
            receivedNetLog message $ "Received message as: " ++ ncOwnUserID ncToPartner ++ " (" ++ ownport ++ ") from: " ++  ncPartnerUserID ncToPartner
            ncIsReady <- isReadyForUse ncToPartner
            if ncIsReady then do
                busy <- SSem.tryWait $ ncHandlingIncomingMessage ncToPartner
                case busy of
                    Just num -> do
                        constate <- MVar.readMVar $ ncConnectionState ncToPartner
                        reply <- case constate of
                            RedirectRequest _ _ host port _ _ _ -> do
                                receivedNetLog message $ "Found redirect request for: " ++ userid
                                receivedNetLog message $ "Send redirect to:" ++ host ++ ":" ++ port
                                SSem.signal $ ncHandlingIncomingMessage ncToPartner
                                NC.sendResponse hdl (Messages.Redirect host port)
                            Connected {} -> case deserialmessages of
                                NewValue userid count val -> do
                                    -- DC.lockInterpreterReads (ncRead ncToPartner)
                                    let fixedPartnerHostAddress = setPartnerHostAddress clientHostaddress val
                                    success <- NB.writeIfNext (ncRead ncToPartner) count fixedPartnerHostAddress
                                    if success then do 
                                        receivedNetLog message "Inserting VChans into VChanCons"
                                        insertVChansIntoVChanCons mvar False fixedPartnerHostAddress
                                        receivedNetLog message "Message written to Channel" 
                                    else receivedNetLog message "Message not correct"
                                    SSem.signal $ ncHandlingIncomingMessage ncToPartner
                                    NC.sendResponse hdl Messages.Okay
                                    receivedNetLog message "Sent okay"
                                    -- DC.unlockInterpreterReads (ncRead ncToPartner)
                                RequestValue userid count -> do
                                    SSem.signal $ ncHandlingIncomingMessage ncToPartner
                                    NC.sendResponse hdl Messages.Okay
                                    mbyval <- NB.tryGetAtNB (NCon.ncWrite ncToPartner) count
                                    Data.Maybe.maybe (return False) (\val -> NO.sendNetworkMessage activeCons ncToPartner (Messages.NewValue (ncOwnUserID ncToPartner) count val) 0) mbyval
                                    return ()
                                AcknowledgeValue userid count -> do
                                    NC.sendResponse hdl Messages.Okay -- This okay is needed here to fix a race-condition with disconnects being faster than the okay
                                    -- NB.serialize (ncWrite ncToPartner) >>= \x -> Config.traceNetIO $ "Online before acknowlegment: " ++ show x
                                    NB.updateAcknowledgements (NCon.ncWrite ncToPartner) count
                                    SSem.signal $ ncHandlingIncomingMessage ncToPartner
                                NewPartnerAddress userid port connectionID -> do
                                    receivedNetLog message $ "Trying to change the address to: " ++ clientHostaddress ++ ":" ++ port
                                    NCon.changePartnerAddress ncToPartner clientHostaddress port connectionID
                                    receivedNetLog message $ "Successfully changed address to: " ++ clientHostaddress ++ ":" ++ port
                                    SSem.signal $ ncHandlingIncomingMessage ncToPartner
                                    NC.sendResponse hdl Messages.Okay

                                    successSendingResponse <- NO.sendNetworkMessage activeCons ncToPartner (Messages.AcknowledgePartnerAddress (ncOwnUserID ncToPartner) connectionID) $ -2
                                    when successSendingResponse $ receivedNetLog message "Successfully acknowledged message"
                                    return ()
                                AcknowledgePartnerAddress userid connectionID -> do
                                    conConfirmed <- NCon.confirmConnectionID ncToPartner connectionID
                                    SSem.signal $ ncHandlingIncomingMessage ncToPartner
                                    if conConfirmed then NC.sendResponse hdl Messages.Okay else NC.sendResponse hdl Messages.Error
                                Disconnect userid -> do
                                    NC.sendResponse hdl Messages.Okay
                                    NCon.disconnectFromPartner ncToPartner
                                    SSem.signal $ ncHandlingIncomingMessage ncToPartner
                                    return ()
                                _ -> do
                                    receivedNetLog message $ "Error unsupported networkmessage: "++ NSerialize.serialize deserialmessages
                                    SSem.signal $ ncHandlingIncomingMessage ncToPartner
                                    NC.sendResponse hdl Messages.Okay
                            _ -> do
                                receivedNetLog message "Network Connection is in a illegal state!"
                                SSem.signal $ ncHandlingIncomingMessage ncToPartner
                                NC.sendResponse hdl Messages.Okay
                        return reply
                    Nothing -> do
                        receivedNetLog message "Message cannot be handled at the moment! Sending wait response"
                        SSem.signal $ ncHandlingIncomingMessage ncToPartner
                        NC.sendResponse hdl Messages.Wait
            else do 
                receivedNetLog message "Found a networkconnection, but it's not ready to be used yet"
                NC.sendResponse hdl Messages.Wait

        Nothing -> do
            receivedNetLog message "Received message from unknown connection"
            case deserialmessages of
                Introduce userid clientport synname syntype -> do
                    serverid <- RandomID.newRandomID
                    newpeer <- newNetworkConnection userid serverid clientHostaddress clientport userid serverid
                    NC.sendResponse hdl (Messages.OkayIntroduce serverid)
                    let repserial = NSerialize.serialize $ Messages.OkayIntroduce serverid
                    receivedNetLog message $ "    Response to "++ userid ++ ": " ++ repserial

                    receivedNetLog message "Patching MVar"
                    netcons <- MVar.takeMVar mvar
                    MVar.putMVar mvar $ Map.insert userid newpeer netcons


                    clientlistraw <- MVar.takeMVar clientlist
                    MVar.putMVar clientlist $ clientlistraw ++ [(userid, (synname, syntype))]
                    -- We must not write clients into the clientlist before adding them to the networkconnectionmap
                
                _ -> do
                    receivedNetLog message $ "Error unsupported networkmessage: " ++ NSerialize.serialize deserialmessages
                    receivedNetLog message "This is probably a timing issue! Lets resend later"
                    NC.sendResponse hdl Messages.Wait


    receivedNetLog message "Message successfully handled"

receivedNetLog :: String -> String -> IO ()
receivedNetLog msg info = Config.traceNetIO $ "Received message: "++msg++" \n    Status: "++info

setPartnerHostAddress ::  String -> Value -> Value
setPartnerHostAddress address = modifyVChansStatic (handleSerial address)
    where
        handleSerial :: String -> Value -> Value
        handleSerial address input = case input of
            VChanSerial r w p o c -> do
                let (hostname, port, partnerID) = c
                VChanSerial r w p o (if hostname == "" then address else hostname, port, partnerID)
            _ -> input -- return input

waitUntilContactedNewPeers :: VChanConnections -> NMC.ActiveConnections -> NetworkConnection Value -> Value -> String -> IO ()
waitUntilContactedNewPeers vchansmvar activeCons ownNC input ownport = do
    contactedPeers <- contactNewPeers vchansmvar activeCons ownport ownNC input
    unless contactedPeers $ do
        threadDelay 50000
        waitUntilContactedNewPeers vchansmvar activeCons ownNC input ownport

contactNewPeers :: VChanConnections -> NMC.ActiveConnections -> String -> NetworkConnection Value ->  Value -> IO Bool
contactNewPeers vchansmvar activeCons ownport ownNC = searchVChans (handleVChan activeCons ownport ownNC) True (&&)
    where
        handleVChan :: NMC.ActiveConnections -> String -> NetworkConnection Value -> Value -> IO Bool
        handleVChan activeCons ownport ownNC input = case input of
            VChan nc bool -> do
                connectionState <- MVar.readMVar $ ncConnectionState nc
                case connectionState of
                    Emulated {} -> return True
                    _ -> do
                        if csConfirmedConnection connectionState then return True else do
                            -- Check whether their partner is also registered and connected on this instance, if so convert the connection into a emulated one
                            success <- NCon.tryConvertToEmulatedConnection vchansmvar nc
                            unless success $ do
                                sendSuccess <- NO.sendNetworkMessage activeCons nc (Messages.NewPartnerAddress (ncOwnUserID nc) ownport $ csOwnConnectionID connectionState) $ -2
                                unless sendSuccess $ threadDelay 100000
                            return success

            _ -> return True

hostaddressTypeToString :: HostAddress -> String
hostaddressTypeToString hostaddress = do
    let (a, b, c, d) = hostAddressToTuple hostaddress
    show a ++ "." ++ show b ++ "."++ show c ++ "." ++ show d

findFittingClientMaybe :: MVar.MVar [(String, (Syntax.Type, Syntax.Type))] -> (Syntax.Type, Syntax.Type) -> IO (Maybe String)
findFittingClientMaybe clientlist desiredType = do
    clientlistraw <- MVar.takeMVar clientlist
    let newclientlistrawAndReturn = fFCMRaw clientlistraw desiredType
    -- We send the name of the type but not the type itself, this needs to change
    MVar.putMVar clientlist $ fst newclientlistrawAndReturn
    return $ snd newclientlistrawAndReturn
    where
        fFCMRaw :: [(String, (Syntax.Type, Syntax.Type))] -> (Syntax.Type, Syntax.Type) -> ([(String, (Syntax.Type, Syntax.Type))], Maybe String)
        fFCMRaw [] _ = ([], Nothing)
        fFCMRaw (x:xs) desiredtype = if compare (snd x) desiredtype then (xs, Just $ fst x) else do
            let nextfFCMRaw = fFCMRaw xs desiredtype
            Data.Bifunctor.first (x :) nextfFCMRaw

        compare :: (Syntax.Type, Syntax.Type) -> (Syntax.Type, Syntax.Type) -> Bool
        compare a@(aName, aType) b@(bName, bType) = aName == Syntax.dualof bName && aType == bType

-- This halts until a fitting client is found
findFittingClient :: MVar.MVar [(String, (Syntax.Type, Syntax.Type))] -> (Syntax.Type, Syntax.Type) -> IO String
findFittingClient clientlist desiredType = do
    mbystring <- findFittingClientMaybe clientlist desiredType
    case mbystring of
        Just userid -> return userid
        Nothing -> do
            threadDelay 10000 -- Sleep for 10 ms to not hammer the CPU
            findFittingClient clientlist desiredType

insertVChansIntoVChanCons :: VChanConnections -> Bool -> Value -> IO ()
insertVChansIntoVChanCons vchansmvar readyForUse = searchVChans (handleSerial vchansmvar readyForUse) () (\_ _ -> ())
    where
        handleSerial :: VChanConnections -> Bool -> Value -> IO ()
        handleSerial vchansmvar readyForUse input = case input of
            VChanSerial r w p o c -> do
                networkconnection <- createNetworkConnection r w p o c
                setReadyForUse networkconnection readyForUse
                Config.traceNetIO $ "Set: " ++ ncOwnUserID networkconnection ++ " not ready for use"
                ncmap <- MVar.takeMVar vchansmvar
                MVar.putMVar vchansmvar $ Map.insert p networkconnection ncmap
                used<- MVar.newEmptyMVar
                MVar.putMVar used False
                return ()
            _ -> return ()

replaceVChanSerial :: NMC.ActiveConnections -> VChanConnections -> Value -> IO Value
replaceVChanSerial activeCons mvar input = modifyVChans (handleSerial activeCons mvar) input
    where
        handleSerial :: NMC.ActiveConnections -> VChanConnections -> Value -> IO Value
        handleSerial activeCons mvar input = case input of
            VChanSerial r w p o c -> do
                ncmap <- MVar.readMVar mvar
                case Map.lookup p ncmap of
                    Nothing -> do
                        -- We simply need to wait for the other thread to finish
                        threadDelay 1000
                        handleSerial activeCons mvar input
                    Just nc -> do 
                        setReadyForUse nc True
                        Config.traceNetIO $ "Set: " ++ ncOwnUserID nc ++ " ready for use"
                        used <- MVar.newMVar False 
                        return $ VChan nc used
            _ -> return input

receiveValue :: VChanConnections -> NMC.ActiveConnections -> NetworkConnection Value -> String -> IO Value
receiveValue vchanconsvar activeCons networkconnection ownport = do
    receiveValueInternal 0 vchanconsvar activeCons networkconnection ownport
    where
        receiveValueInternal :: Int -> VChanConnections -> NMC.ActiveConnections -> NetworkConnection Value -> String -> IO Value
        receiveValueInternal count vchanconsvar activeCons networkconnection ownport = do
            let readDC = ncRead networkconnection
            mbyUnclean <- NB.tryTake readDC
            case mbyUnclean of
                Just unclean -> do
                    val <- replaceVChanSerial activeCons vchanconsvar $ fst unclean
                    waitUntilContactedNewPeers vchanconsvar activeCons networkconnection val ownport
                    -- msgCount <- DC.unreadMessageStart $ ncRead networkconnection
                    connectionState <- MVar.readMVar $ ncConnectionState networkconnection
                    waitTillAcknowledged vchanconsvar activeCons networkconnection $ snd unclean

                    return val
                Nothing -> if count == 0 then do
                        msgCount <- NB.getNextOffset $ ncRead networkconnection
                        connectionState <- MVar.readMVar $ ncConnectionState networkconnection
                        case connectionState of
                            Connected {} -> NO.sendNetworkMessage activeCons networkconnection (Messages.RequestValue (ncOwnUserID networkconnection) msgCount) $ -2
                            _ -> return True
                        receiveValueInternal 100 vchanconsvar activeCons networkconnection ownport
                        else do
                            threadDelay 5000
                            receiveValueInternal (count-1) vchanconsvar activeCons networkconnection ownport

waitTillAcknowledged :: VChanConnections -> NMC.ActiveConnections -> NetworkConnection Value -> Int -> IO ()
waitTillAcknowledged vcv ac nc vaToAck = do 
    success <- tryToAcknowledgeValue vcv ac nc vaToAck
    unless success $ waitTillAcknowledged vcv ac nc vaToAck

-- We need to seperate this in case the state of the connection changes to emulated
tryToAcknowledgeValue :: VChanConnections -> NMC.ActiveConnections -> NetworkConnection Value -> Int -> IO Bool
tryToAcknowledgeValue vchanconsvar activeCons networkconnection valueToAcknowledge = do
    connectionState <- MVar.readMVar $ ncConnectionState networkconnection
    case connectionState of
        Connected {} -> do 
            success <- NO.sendNetworkMessage activeCons networkconnection (Messages.AcknowledgeValue (ncOwnUserID networkconnection) valueToAcknowledge) $ -2
            unless success $ threadDelay 100000 -- If sending is not successful give the other party some time to recover
            return success
        Emulated {} -> do
            vchancons <- MVar.readMVar vchanconsvar
            let ownid = ncOwnUserID networkconnection
            let mbypartner = Map.lookup ownid vchancons
            case mbypartner of
                Just partner -> do
                    -- NB.serialize (ncWrite partner) >>= \x -> Config.traceNetIO $ "Emulated "++ show unclean ++ " before acknowledgment: " ++ show x
                    NB.updateAcknowledgements (ncWrite partner) valueToAcknowledge
                    return True
                _ -> Config.traceNetIO "Something went wrong when acknowledging value of emulated connection" >> return False
        _ -> return True