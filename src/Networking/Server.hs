{-# LANGUAGE LambdaCase #-}
module Networking.Server where

import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.Chan as Chan
import Control.Monad.IO.Class
import qualified Data.Map as Map
import qualified Data.Maybe
import GHC.IO.Handle
import Network.Socket
import Control.Concurrent

import Networking.Messages
import qualified ValueParsing.ValueTokens as VT
import qualified ValueParsing.ValueGrammar as VG
import qualified Networking.Common as NC
import qualified Networking.Serialize as NSerialize
import ProcessEnvironmentTypes
import qualified Syntax

import Control.Exception
import qualified Networking.UserID as UserID
import qualified Networking.Messages as Messages
import qualified Networking.DirectionalConnection as ND
import qualified Networking.Client as NClient

import Networking.NetworkConnection
import qualified Networking.Common as NC
import qualified Config
import qualified Networking.NetworkConnection as NCon
import qualified Control.Concurrent as MVar
import qualified Networking.Client as NC
import Control.Monad

import qualified Networking.NetworkingMethod.NetworkingMethodCommon as NMC
import qualified Control.Concurrent.SSem as SSem

handleClient :: NMC.ActiveConnections -> MVar.MVar (Map.Map String (NetworkConnection Value)) -> MVar.MVar [(String, Syntax.Type)] -> (Socket, SockAddr) -> NC.ConversationOrHandle -> String -> String -> Messages -> IO ()
handleClient activeCons mvar clientlist clientsocket hdl ownport message deserialmessages = do
    let userid = getUserID deserialmessages
    netcon <- MVar.readMVar mvar

    clientHostaddress <- case snd clientsocket of
        SockAddrInet _ hostname -> return $ hostaddressTypeToString hostname
        _ -> return ""

    -- MVar.putMVar mvar netcon
    case Map.lookup userid netcon of 
        Just networkcon -> do -- SSem.withSem (ncHandlingIncomingMessage networkcon) $ do
            Config.traceNetIO $ "Recieved message as: " ++ Data.Maybe.fromMaybe "" (ncOwnUserID networkcon) ++ " (" ++ ownport ++ ") from: " ++  Data.Maybe.fromMaybe "" (ncPartnerUserID networkcon)
            Config.traceNetIO $ "    "++message
            busy <- SSem.tryWait $ ncHandlingIncomingMessage networkcon
            case busy of
                Just num -> do
                    redirectRequest <- checkAndSendRedirectRequest hdl netcon userid
                    unless redirectRequest $
                        case deserialmessages of
                            NewValue userid count val -> do
                                handleNewValue activeCons mvar userid count val ownport clientHostaddress hdl
                            IntroduceClient userid clientport syntype-> do
                                handleIntroduceClient mvar clientlist clientsocket hdl userid clientport syntype
                            RequestSync userid count -> do
                                handleRequestSync mvar userid count hdl
                            SyncIncoming userid values -> do
                                handleSyncIncoming mvar userid values
                                NC.sendResponse hdl Messages.Okay
                            IntroduceNewPartnerAddress userid port -> do
                                networkconnectionmap <- MVar.takeMVar mvar
                                Config.traceNetIO $ "Took MVar for message: " ++ message
                                case Map.lookup userid networkconnectionmap of
                                    Just networkconnection -> do  -- Change to current network address
                                        case snd clientsocket of
                                            SockAddrInet _ hostname -> do 
                                                Config.traceNetIO $ "Trying to change the address to: " ++ hostaddressTypeToString hostname ++ ":" ++ port
                                                NCon.changePartnerAddress networkconnection (hostaddressTypeToString hostname) port
                                            _ -> return ()
                                        Config.traceNetIO $ "Put MVar for message: " ++ message
                                        MVar.putMVar mvar networkconnectionmap
                                    Nothing -> MVar.putMVar mvar networkconnectionmap  -- Nothing needs to be done here, the connection hasn't been established yet. No need to save that
                                NC.sendResponse hdl Messages.Okay
                            _ -> do
                                serial <- NSerialize.serialize deserialmessages
                                Config.traceIO $ "Error unsupported networkmessage: "++ serial
                                NC.sendResponse hdl Messages.Okay
                    SSem.signal $ ncHandlingIncomingMessage networkcon
                Nothing -> do
                        Config.traceNetIO "Message cannot be handled at the moment! Sending wait response"
                        NC.sendResponse hdl Messages.Wait
        Nothing -> do
            Config.traceNetIO "Recieved message from unknown connection!"
            case deserialmessages of
                IntroduceClient userid clientport syntype-> do
                    handleIntroduceClient mvar clientlist clientsocket hdl userid clientport syntype
                _ -> do
                    serial <- NSerialize.serialize deserialmessages
                    Config.traceIO $ "Error unsupported networkmessage: "++ serial
                    Config.traceIO "This is probably a timing issue! Lets resend later"
                    NC.sendResponse hdl Messages.Wait
    Config.traceNetIO $ "    Message: " ++ message

    

checkAndSendRedirectRequest :: NC.ConversationOrHandle -> Map.Map String (NetworkConnection Value) -> String -> IO Bool
checkAndSendRedirectRequest handle ncmap userid = do
    case Map.lookup userid ncmap of
        Nothing -> return False
        Just networkconnection -> do
            constate <- MVar.readMVar $ ncConnectionState networkconnection
            case constate of
                RedirectRequest _ _ host port -> do 
                    Config.traceNetIO $ "Found redirect request for: " ++ userid
                    Config.traceNetIO $ "Send redirect to:" ++ host ++ ":" ++ port
                    NC.sendResponse handle (Messages.Redirect host port)
                    return True
                _ -> return False

handleNewValue :: NMC.ActiveConnections -> MVar.MVar (Map.Map String (NetworkConnection Value)) -> String -> Int -> Value -> String -> String -> NC.ConversationOrHandle -> IO ()
handleNewValue activeCons mvar userid count rawval ownport partneraddress hdl = do
    -- networkconnectionmap <- MVar.takeMVar mvar
    let val = setPartnerHostAddress partneraddress rawval
    networkconnectionmap <- MVar.readMVar mvar
    case Map.lookup userid networkconnectionmap of
        Just networkconnection -> do
            ND.lockInterpreterReads (ncRead networkconnection)
            success <- ND.writeMessageIfNext (ncRead networkconnection) count val
            incomingCount <- ND.countMessages (ncRead networkconnection)
            unless success $ NC.sendNetworkMessage activeCons networkconnection (Messages.RequestSync (Data.Maybe.fromMaybe "" (ncOwnUserID networkconnection)) incomingCount) (-1)
            contactNewPeers activeCons val ownport
            NC.sendResponse hdl Messages.Okay
            ND.unlockInterpreterReads (ncRead networkconnection)
        Nothing -> do
            NC.sendResponse hdl Messages.Okay
            Config.traceNetIO "Error during recieving a networkmessage: Introduction is needed prior to sending values!"
    -- MVar.putMVar mvar networkconnectionmap


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
        let (hostname, port) = c
        VChanSerial r w p o (if hostname == "" then address else hostname, port)
    _ -> input -- return input
    where
        setPartnerHostAddressPEnv :: String -> [(String, Value)] -> [(String, Value)]
        setPartnerHostAddressPEnv _ [] = []
        setPartnerHostAddressPEnv clientHostaddress penvs@(x:xs) =
            let newval = setPartnerHostAddress clientHostaddress $ snd x in
            (fst x, newval):setPartnerHostAddressPEnv clientHostaddress xs


contactNewPeers :: NMC.ActiveConnections -> Value -> String -> IO ()
contactNewPeers activeCons input ownport = case input of
    VSend v -> do
        nv <- contactNewPeers activeCons v ownport
        -- return $ VSend nv
        return ()
    VPair v1 v2 -> do
        nv1 <- contactNewPeers activeCons v1 ownport
        nv2 <- contactNewPeers activeCons v2 ownport
        -- return $ VPair nv1 nv2
        return () 
    VFunc penv a b -> do
        newpenv <- contactNewPeersPEnv activeCons penv ownport
        -- return $ VFunc newpenv a b
        return ()
    VDynCast v g -> do
        nv <- contactNewPeers activeCons v ownport
        -- return $ VDynCast nv g
        return ()
    VFuncCast v a b -> do
        nv <- contactNewPeers activeCons v ownport
        -- return $ VFuncCast nv a b
        return ()
    VRec penv a b c d -> do
        newpenv <- contactNewPeersPEnv activeCons penv ownport
        -- return $ VRec newpenv a b c d
        return ()
    VNewNatRec penv a b c d e f g -> do
        newpenv <- contactNewPeersPEnv activeCons penv ownport
        -- return $ VNewNatRec newpenv a b c d e f g
        return ()
    VChanSerial r w p o c -> do
        let (hostname, port) = c
        tempNC <- newNetworkConnection p o hostname port
        NClient.sendNetworkMessage activeCons tempNC (Messages.IntroduceNewPartnerAddress o ownport) 5
    _ -> return () -- return input
    where
        contactNewPeersPEnv :: NMC.ActiveConnections -> [(String, Value)] -> String -> IO () -- [(String, Value)]
        contactNewPeersPEnv _ [] _ = return () --return []
        contactNewPeersPEnv activeCons (x:xs) ownport = do
            newval <- contactNewPeers activeCons (snd x) ownport
            rest <- contactNewPeersPEnv activeCons xs ownport
            -- return $ (fst x, newval):rest
            return ()

handleIntroduceClient :: MVar.MVar (Map.Map String (NetworkConnection Value)) -> MVar.MVar [(String, Syntax.Type)] -> (Socket, SockAddr) -> NC.ConversationOrHandle -> String -> String -> Syntax.Type -> IO ()
handleIntroduceClient mvar clientlist clientsocket hdl userid clientport syntype = do
    networkconnectionmap <- MVar.takeMVar mvar
    case Map.lookup userid networkconnectionmap of
        Just networkconnection -> do
            Config.traceIO "Error during recieving a networkmessage: Already introduced to this client!"
            MVar.putMVar mvar networkconnectionmap
        Nothing ->  case snd clientsocket of -- This client is new
            SockAddrInet port hostname -> do
                serverid <- UserID.newRandomUserID
                networkconnection <- newNetworkConnection userid serverid (hostaddressTypeToString hostname) clientport
                let newnetworkconnectionmap = Map.insert userid networkconnection networkconnectionmap
                MVar.putMVar mvar newnetworkconnectionmap
                NC.sendResponse hdl (Messages.OkayIntroduce serverid)
                repserial <- NSerialize.serialize $ Messages.OkayIntroduce serverid
                Config.traceNetIO $ "    Response to "++ userid ++ ": " ++ repserial
                -- Adds the new user to the users that can be accepted by the server
                clientlistraw <- MVar.takeMVar clientlist
                MVar.putMVar clientlist $ clientlistraw ++ [(userid, syntype)]

            _ -> do
                Config.traceIO "Error during recieving a networkmessage: only ipv4 is currently supported!"
                MVar.putMVar mvar networkconnectionmap
                NC.sendResponse hdl Messages.Okay 

handleChangePartnerAddress :: NMC.ActiveConnections -> MVar.MVar (Map.Map String (NetworkConnection Value)) -> String -> String -> String -> String -> IO ()
handleChangePartnerAddress activeCons mvar userid hostname port ownport = do
    networkconnectionmap <- MVar.takeMVar mvar
    case Map.lookup userid networkconnectionmap of
        Just networkconnection -> do  -- Change to current network address
            NCon.changePartnerAddress networkconnection hostname port
            -- For some reason constate doesn't seem to properly apply

            NClient.sendNetworkMessage activeCons networkconnection (Messages.IntroduceNewPartnerAddress (Data.Maybe.fromMaybe "" (ncOwnUserID networkconnection)) ownport) 5
            MVar.putMVar mvar networkconnectionmap

        Nothing -> MVar.putMVar mvar networkconnectionmap  -- Nothing needs to be done here, the connection hasn't been established yet. No need to save that

handleRequestSync :: MVar.MVar (Map.Map String (NetworkConnection Value)) -> String -> Int -> NC.ConversationOrHandle -> IO ()
handleRequestSync mvar userid count hdl = do
    networkconnectionmap <- MVar.readMVar mvar
    case Map.lookup userid networkconnectionmap of
        Just networkconnection -> do  -- Change to current network address
            writevals <- ND.allMessages $ ncWrite networkconnection
            if length writevals > count then NC.sendResponse hdl (Messages.OkaySync writevals) else NC.sendResponse hdl Messages.Okay
        othing -> return ()

handleSyncIncoming :: MVar.MVar (Map.Map String (NetworkConnection Value)) -> String -> [Value] -> IO ()
handleSyncIncoming mvar userid values = do
    networkconnectionmap <- MVar.readMVar mvar
    case Map.lookup userid networkconnectionmap of
        Just networkconnection -> do  -- Change to current network address
            ND.syncMessages (ncRead networkconnection) values
        Nothing -> return ()

hostaddressTypeToString :: HostAddress -> String
hostaddressTypeToString hostaddress = do
    let (a, b, c, d) = hostAddressToTuple hostaddress
    show a ++ "." ++ show b ++ "."++ show c ++ "." ++ show d

findFittingClientMaybe :: MVar.MVar [(String, Syntax.Type)] -> Syntax.Type -> IO (Maybe String)
findFittingClientMaybe clientlist desiredType = do
    clientlistraw <- MVar.takeMVar clientlist
    let newclientlistrawAndReturn = fFCMRaw clientlistraw desiredType
    -- We send the name of the type but not the type itself, this needs to change
    MVar.putMVar clientlist $ fst newclientlistrawAndReturn
    return $ snd newclientlistrawAndReturn
    where
        fFCMRaw :: [(String, Syntax.Type)] -> Syntax.Type -> ([(String, Syntax.Type)], Maybe String)
        fFCMRaw [] _ = ([], Nothing)
        fFCMRaw (x:xs) desiredtype = if snd x == Syntax.dualof desiredtype then (xs, Just $ fst x) else do
            let nextfFCMRaw = fFCMRaw xs desiredtype
            (x:(fst nextfFCMRaw), snd nextfFCMRaw)

-- This halts until a fitting client is found
findFittingClient :: MVar.MVar [(String, Syntax.Type)] -> Syntax.Type -> IO String
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
        networkconnection <- createNetworkConnectionS r w p o c
        ncmap <- MVar.takeMVar mvar
        MVar.putMVar mvar $ Map.insert p networkconnection ncmap
        NClient.sendNetworkMessage activeCons networkconnection (RequestSync o $ length r) 5
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
