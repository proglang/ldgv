{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
module Networking.Server where

import qualified Control.Concurrent.MVar as MVar
import qualified Data.Map as Map
import qualified Data.Maybe
import Network.Socket
import Control.Concurrent

import Networking.Messages
import qualified Networking.Common as NC
import qualified Networking.Serialize as NSerialize
import ProcessEnvironmentTypes
import qualified Syntax

import qualified Networking.RandomID as RandomID
import qualified Networking.Messages as Messages
import qualified Networking.Client as NClient

import Networking.NetworkConnection
import qualified Config
import qualified Networking.NetworkConnection as NCon
import Control.Monad

import qualified Networking.NetworkingMethod.NetworkingMethodCommon as NMC
import qualified Control.Concurrent.SSem as SSem
import qualified Networking.DirectionalConnection as DC
import qualified Data.Bifunctor
import qualified Networking.NetworkBuffer as NB

handleClient :: NMC.ActiveConnections -> MVar.MVar (Map.Map String (NetworkConnection Value)) -> MVar.MVar [(String, (Syntax.Type, Syntax.Type))] -> (Socket, SockAddr) -> NC.ConversationOrHandle -> String -> String -> Message -> IO ()
handleClient activeCons mvar clientlist clientsocket hdl ownport message deserialmessages = do
    let userid = getUserID deserialmessages
    clientHostaddress <- case snd clientsocket of
        SockAddrInet _ hostname -> return $ hostaddressTypeToString hostname
        _ -> do
            recievedNetLog message "Error during recieving a networkmessage: only ipv4 is currently supported!"
            return ""

    netcons <- MVar.readMVar mvar
    case Map.lookup userid netcons of
        Just networkcon -> do
            recievedNetLog message $ "Recieved message as: " ++ ncOwnUserID networkcon ++ " (" ++ ownport ++ ") from: " ++  ncPartnerUserID networkcon
            busy <- SSem.tryWait $ ncHandlingIncomingMessage networkcon
            case busy of
                Just num -> do
                    constate <- MVar.readMVar $ ncConnectionState networkcon
                    reply <- case constate of
                        RedirectRequest _ _ host port _ _ _ -> do
                            recievedNetLog message $ "Found redirect request for: " ++ userid
                            recievedNetLog message $ "Send redirect to:" ++ host ++ ":" ++ port
                            SSem.signal $ ncHandlingIncomingMessage networkcon
                            NC.sendResponse hdl (Messages.Redirect host port)
                        Connected {} -> case deserialmessages of
                            NewValue userid count val -> do
                                -- DC.lockInterpreterReads (ncRead networkcon)
                                success <- NB.writeIfNext (ncRead networkcon) count $ setPartnerHostAddress clientHostaddress val
                                SSem.signal $ ncHandlingIncomingMessage networkcon
                                if success then recievedNetLog message "Message written to Channel" else recievedNetLog message "Message not correct"
                                NC.sendResponse hdl Messages.Okay
                                recievedNetLog message "Sent okay"
                                -- DC.unlockInterpreterReads (ncRead networkcon)
                            RequestValue userid count -> do
                                SSem.signal $ ncHandlingIncomingMessage networkcon
                                NC.sendResponse hdl Messages.Okay
                                mbyval <- NB.tryGetAtNB (NCon.ncWrite networkcon) count
                                Data.Maybe.maybe (return False) (\val -> NClient.sendNetworkMessage activeCons networkcon (Messages.NewValue (ncOwnUserID networkcon) count val) 0) mbyval
                                return ()
                            AcknowledgeValue userid count -> do
                                NC.sendResponse hdl Messages.Okay -- This okay is needed here to fix a race-condition with disconnects being faster than the okay
                                NB.updateAcknowledgements (NCon.ncWrite networkcon) count
                                SSem.signal $ ncHandlingIncomingMessage networkcon
                            NewPartnerAddress userid port connectionID -> do
                                recievedNetLog message $ "Trying to change the address to: " ++ clientHostaddress ++ ":" ++ port
                                NCon.changePartnerAddress networkcon clientHostaddress port connectionID
                                SSem.signal $ ncHandlingIncomingMessage networkcon
                                NC.sendResponse hdl Messages.Okay
                                NClient.sendNetworkMessage activeCons networkcon (Messages.AcknowledgePartnerAddress (ncOwnUserID networkcon) connectionID) 0
                                return ()
                            AcknowledgePartnerAddress userid connectionID -> do
                                conConfirmed <- NCon.confirmConnectionID networkcon connectionID
                                SSem.signal $ ncHandlingIncomingMessage networkcon
                                if conConfirmed then NC.sendResponse hdl Messages.Okay else NC.sendResponse hdl Messages.Error
                            Disconnect userid -> do
                                NCon.disconnectFromPartner networkcon
                                SSem.signal $ ncHandlingIncomingMessage networkcon
                                NC.sendResponse hdl Messages.Okay
                            _ -> do
                                serial <- NSerialize.serialize deserialmessages
                                recievedNetLog message $ "Error unsupported networkmessage: "++ serial
                                SSem.signal $ ncHandlingIncomingMessage networkcon
                                NC.sendResponse hdl Messages.Okay
                        _ -> do
                            recievedNetLog message "Network Connection is in a illegal state!"
                            SSem.signal $ ncHandlingIncomingMessage networkcon
                            NC.sendResponse hdl Messages.Okay
                    return reply
                Nothing -> do
                    recievedNetLog message "Message cannot be handled at the moment! Sending wait response"
                    SSem.signal $ ncHandlingIncomingMessage networkcon
                    NC.sendResponse hdl Messages.Wait

        Nothing -> do
            recievedNetLog message "Recieved message from unknown connection"
            case deserialmessages of
                IntroduceClient userid clientport synname syntype -> do
                    serverid <- RandomID.newRandomID
                    newpeer <- newNetworkConnection userid serverid clientHostaddress clientport userid serverid
                    NC.sendResponse hdl (Messages.OkayIntroduce serverid)
                    repserial <- NSerialize.serialize $ Messages.OkayIntroduce serverid
                    recievedNetLog message $ "    Response to "++ userid ++ ": " ++ repserial

                    recievedNetLog message "Patching MVar"
                    netcons <- MVar.takeMVar mvar
                    MVar.putMVar mvar $ Map.insert userid newpeer netcons


                    clientlistraw <- MVar.takeMVar clientlist
                    MVar.putMVar clientlist $ clientlistraw ++ [(userid, (synname, syntype))]
                    -- We must not write clients into the clientlist before adding them to the networkconnectionmap
                _ -> do
                    serial <- NSerialize.serialize deserialmessages
                    recievedNetLog message $ "Error unsupported networkmessage: "++ serial
                    recievedNetLog message "This is probably a timing issue! Lets resend later"
                    NC.sendResponse hdl Messages.Wait


    recievedNetLog message "Message successfully handled"

recievedNetLog :: String -> String -> IO ()
recievedNetLog msg info = Config.traceNetIO $ "Recieved message: "++msg++" \n    Status: "++info

setPartnerHostAddress ::  String -> Value -> Value
setPartnerHostAddress address input = case input of
    VSend v -> VSend $ setPartnerHostAddress address v
    VPair v1 v2 ->
        let nv1 = setPartnerHostAddress address v1 in
        let nv2 = setPartnerHostAddress address v2 in
        VPair nv1 nv2
    VFunc penv a b ->
        let newpenv = setPartnerHostAddressPEnv address penv in
        VFunc newpenv a b
    VDynCast v g -> VDynCast (setPartnerHostAddress address v) g
    VFuncCast v a b -> VFuncCast (setPartnerHostAddress address v) a b
    VRec penv a b c d ->
        let newpenv = setPartnerHostAddressPEnv address penv in
        VRec newpenv a b c d
    VNewNatRec penv a b c d e f g ->
        let newpenv = setPartnerHostAddressPEnv address penv in
        VNewNatRec newpenv a b c d e f g
    VChanSerial r w p o c -> do
        let (hostname, port, partnerID) = c
        VChanSerial r w p o (if hostname == "" then address else hostname, port, partnerID)
    _ -> input -- return input
    where
        setPartnerHostAddressPEnv :: String -> [(String, Value)] -> [(String, Value)]
        setPartnerHostAddressPEnv _ [] = []
        setPartnerHostAddressPEnv clientHostaddress penvs@(x:xs) =
            let newval = setPartnerHostAddress clientHostaddress $ snd x in
            (fst x, newval):setPartnerHostAddressPEnv clientHostaddress xs

waitUntilContactedNewPeers :: NMC.ActiveConnections -> Value -> String -> IO ()
waitUntilContactedNewPeers activeCons input ownport = do
    contactedPeers <- contactNewPeers activeCons input ownport
    unless contactedPeers $ do
        threadDelay 50000
        waitUntilContactedNewPeers activeCons input ownport


contactNewPeers :: NMC.ActiveConnections -> Value -> String -> IO Bool
contactNewPeers activeCons input ownport = case input of
    VSend v -> do
        contactNewPeers activeCons v ownport
    VPair v1 v2 -> do
        nv1 <- contactNewPeers activeCons v1 ownport
        nv2 <- contactNewPeers activeCons v2 ownport
        return (nv1 || nv2)
    VFunc penv a b -> do
        contactNewPeersPEnv activeCons penv ownport
    VDynCast v g -> do
        contactNewPeers activeCons v ownport
    VFuncCast v a b -> do
        contactNewPeers activeCons v ownport
    VRec penv a b c d -> do
        contactNewPeersPEnv activeCons penv ownport
    VNewNatRec penv a b c d e f g -> do
        contactNewPeersPEnv activeCons penv ownport
    VChan nc bool -> do
        connectionState <- MVar.readMVar $ ncConnectionState nc
        case connectionState of
            Emulated {} -> return True
            _ -> do
                if csConfirmedConnection connectionState then return True else do
                    NClient.sendNetworkMessage activeCons nc (Messages.NewPartnerAddress (ncOwnUserID nc) ownport $ csOwnConnectionID connectionState) 0
                    return False
    _ -> return True
    where
        contactNewPeersPEnv :: NMC.ActiveConnections -> [(String, Value)] -> String -> IO Bool -- [(String, Value)]
        contactNewPeersPEnv _ [] _ = return True
        contactNewPeersPEnv activeCons (x:xs) ownport = do
            newval <- contactNewPeers activeCons (snd x) ownport
            rest <- contactNewPeersPEnv activeCons xs ownport
            -- return $ (fst x, newval):rest
            return (newval || rest)

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

replaceVChanSerial :: NMC.ActiveConnections -> MVar.MVar (Map.Map String (NetworkConnection Value)) -> Value -> IO Value
replaceVChanSerial activeCons mvar input = case input of
    VSend v -> do
        nv <- replaceVChanSerial activeCons mvar v
        return $ VSend nv
    VPair v1 v2 -> do
        nv1 <- replaceVChanSerial activeCons mvar v1
        nv2 <- replaceVChanSerial activeCons mvar v2
        return $ VPair nv1 nv2
    VFunc penv a b -> do
        newpenv <- replaceVChanSerialPEnv activeCons mvar penv
        return $ VFunc newpenv a b
    VDynCast v g -> do
        nv <- replaceVChanSerial activeCons mvar v
        return $ VDynCast nv g
    VFuncCast v a b -> do
        nv <- replaceVChanSerial activeCons mvar v
        return $ VFuncCast nv a b
    VRec penv a b c d -> do
        newpenv <- replaceVChanSerialPEnv activeCons mvar penv
        return $ VRec newpenv a b c d
    VNewNatRec penv a b c d e f g -> do
        newpenv <- replaceVChanSerialPEnv activeCons mvar penv
        return $ VNewNatRec newpenv a b c d e f g
    VChanSerial r w p o c -> do
        networkconnection <- createNetworkConnection r w p o c
        ncmap <- MVar.takeMVar mvar
        MVar.putMVar mvar $ Map.insert p networkconnection ncmap
        used<- MVar.newEmptyMVar
        MVar.putMVar used False
        return $ VChan networkconnection used
    _ -> return input
    where
        replaceVChanSerialPEnv :: NMC.ActiveConnections -> MVar.MVar (Map.Map String (NetworkConnection Value)) -> [(String, Value)] -> IO [(String, Value)]
        replaceVChanSerialPEnv _ _ [] = return []
        replaceVChanSerialPEnv activeCons mvar (x:xs) = do
            newval <- replaceVChanSerial activeCons mvar $ snd x
            rest <- replaceVChanSerialPEnv activeCons mvar xs
            return $ (fst x, newval):rest

recieveValue :: VChanConnections -> NMC.ActiveConnections -> NetworkConnection Value -> String -> IO Value
recieveValue vchanconsvar activeCons networkconnection ownport = do
    connectionState <- MVar.readMVar $ ncConnectionState networkconnection
    case connectionState of
        Emulated {} -> recieveValueEmulated vchanconsvar activeCons networkconnection ownport
        _ -> recieveValueInternal 0 vchanconsvar activeCons networkconnection ownport
    where
        recieveValueInternal :: Int -> VChanConnections -> NMC.ActiveConnections -> NetworkConnection Value -> String -> IO Value
        recieveValueInternal count vchanconsvar activeCons networkconnection ownport = do
            let readDC = ncRead networkconnection
            mbyUnclean <- NB.tryTake readDC
            case mbyUnclean of
                Just unclean -> do
                    val <- replaceVChanSerial activeCons vchanconsvar $ fst unclean
                    waitUntilContactedNewPeers activeCons val ownport
                    -- msgCount <- DC.unreadMessageStart $ ncRead networkconnection
                    NClient.sendNetworkMessage activeCons networkconnection (Messages.AcknowledgeValue (ncOwnUserID networkconnection) $ snd unclean) $ -1
                    return val
                Nothing -> if count == 0 then do
                        msgCount <- NB.getNextOffset $ ncRead networkconnection
                        NClient.sendNetworkMessage activeCons networkconnection (Messages.RequestValue (ncOwnUserID networkconnection) msgCount) 0
                        recieveValueInternal 100 vchanconsvar activeCons networkconnection ownport
                        else do
                            threadDelay 5000
                            recieveValueInternal (count-1) vchanconsvar activeCons networkconnection ownport
        recieveValueEmulated :: VChanConnections -> NMC.ActiveConnections -> NetworkConnection Value -> String -> IO Value
        recieveValueEmulated vchanconsvar activeCons networkconnection ownport = do
            let readDC = ncRead networkconnection
            mbyUnclean <- NB.tryTake readDC
            case mbyUnclean of
                Just unclean -> do
                    val <- replaceVChanSerial activeCons vchanconsvar $ fst unclean
                    waitUntilContactedNewPeers activeCons val ownport
                    case val of
                        VChan nc _ -> do
                            connectionState <- MVar.readMVar $ ncConnectionState nc
                            Config.traceNetIO $ show connectionState
                        _ -> return ()

                    -- msgCount <- DC.unreadMessageStart $ ncRead networkconnection
                    vchancons <- MVar.readMVar vchanconsvar
                    let ownid = ncOwnUserID networkconnection
                    let mbypartner = Map.lookup ownid vchancons
                    case mbypartner of
                        Just partner -> do
                            NB.updateAcknowledgements (ncWrite partner) $ snd unclean
                            return ()
                        _ -> Config.traceNetIO "Something went wrong when acknowleding value of emulated connection"

                    return val
                Nothing -> do
                    threadDelay 5000
                    recieveValueEmulated vchanconsvar activeCons networkconnection ownport

