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
import ProcessEnvironment
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
import ProcessEnvironment (ServerSocket)
import qualified Networking.Client as NC
import Control.Monad

createServer :: Int -> IO (MVar.MVar (Map.Map String (NetworkConnection Value)), MVar.MVar [(String, Syntax.Type)])
createServer port = do
    serverid <- UserID.newRandomUserID
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    let hints = defaultHints {
            addrFamily = AF_INET
          , addrFlags = [AI_PASSIVE]
          , addrSocketType = Stream
    }
    addrInfo <- getAddrInfo (Just hints) Nothing $ Just $ show port
    bind sock $ addrAddress $ head addrInfo
    listen sock 1024
    mvar <- MVar.newEmptyMVar
    MVar.putMVar mvar Map.empty
    clientlist <- MVar.newEmptyMVar
    MVar.putMVar clientlist []
    forkIO $ acceptClients mvar clientlist sock $ show port
    return (mvar, clientlist)

acceptClients :: MVar.MVar (Map.Map String (NetworkConnection Value)) -> MVar.MVar [(String, Syntax.Type)] -> Socket -> String -> IO ()
acceptClients mvar clientlist socket ownport = do
    Config.traceIO "Waiting for clients"
    clientsocket <- accept socket
    Config.traceIO "Accepted new client"

    forkIO $ acceptClient mvar clientlist clientsocket ownport
    acceptClients mvar clientlist socket ownport

-- In the nothing case we shoud wait a few seconds for other messages to resolve the issue
acceptClient :: MVar.MVar (Map.Map String (NetworkConnection Value)) -> MVar.MVar [(String, Syntax.Type)] -> (Socket, SockAddr) -> String -> IO ()
acceptClient mvar clientlist clientsocket ownport = do
    hdl <- NC.getHandle $ fst clientsocket
    -- NC.recieveMessage hdl VG.parseMessages (\_ -> hClose hdl) (\msg des -> void $ forkIO ( do 
    --    handleClient mvar clientlist clientsocket hdl ownport msg des
    --     hClose hdl
    --    ))
    NC.recieveMessage hdl VG.parseMessages (\_ -> return ()) $ handleClient mvar clientlist clientsocket hdl ownport
    hClose hdl

handleClient :: MVar.MVar (Map.Map String (NetworkConnection Value)) -> MVar.MVar [(String, Syntax.Type)] -> (Socket, SockAddr) -> Handle -> String -> String -> Messages -> IO ()
handleClient mvar clientlist clientsocket hdl ownport message deserialmessages = do
    let userid = getUserID deserialmessages
    Config.traceNetIO $ show ownport ++ " Entering redirect handler for message: "++ message
    netcon <- MVar.takeMVar mvar
    Config.traceNetIO $ show ownport ++ " Entered redirect handler for message: "++ message
    redirectRequest <- checkRedirectRequest netcon userid
    Config.traceNetIO $ show ownport ++ " Redirect request" ++ show redirectRequest
    Config.traceNetIO $ show ownport ++ " Leaving redirect handler for message: " ++ message 
    MVar.putMVar mvar netcon
    Config.traceNetIO $ show ownport ++ " Left redirect handler for message: " ++ message 
    case Map.lookup userid netcon of 
        Just networkcon -> do
            Config.traceNetIO $ "Recieved message as: " ++ Data.Maybe.fromMaybe "" (ncOwnUserID networkcon) ++ " (" ++ ownport ++ ") from: " ++  Data.Maybe.fromMaybe "" (ncPartnerUserID networkcon)
            if redirectRequest then sendRedirect hdl netcon userid else do
                case deserialmessages of
                    NewValue userid count val -> do
                        handleNewValue mvar userid count val ownport hdl
                    IntroduceClient userid clientport syntype-> do
                        handleIntroduceClient mvar clientlist clientsocket hdl userid clientport syntype
                        -- Okay message is handled in handle introduce
                    ChangePartnerAddress userid hostname port -> do
                        handleChangePartnerAddress mvar userid hostname port ownport
                        NC.sendMessage Messages.Okay hdl
                    RequestSync userid -> do
                        handleRequestSync mvar userid hdl
                        -- NC.sendMessage Messages.Okay hdl
                    SyncIncoming userid values -> do
                        handleSyncIncoming mvar userid values
                        NC.sendMessage Messages.Okay hdl
                    RequestClose userid -> do
                        handleRequestClose mvar userid
                        NC.sendMessage Messages.Okay hdl
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
                                -- For some reason constate doesn't seem to properly apply                        MVar.putMVar mvar networkconnectionmap

                            Nothing -> MVar.putMVar mvar networkconnectionmap  -- Nothing needs to be done here, the connection hasn't been established yet. No need to save that
                        NC.sendMessage Messages.Okay hdl

                    _ -> do
                        serial <- NSerialize.serialize deserialmessages
                        Config.traceIO $ "Error unsupported networkmessage: "++ serial
                        NC.sendMessage Messages.Okay hdl
        Nothing -> do
            Config.traceNetIO "Recieved message from unknown connection!"
            if redirectRequest then sendRedirect hdl netcon userid else do
                case deserialmessages of
                    IntroduceClient userid clientport syntype-> do
                        handleIntroduceClient mvar clientlist clientsocket hdl userid clientport syntype
                        -- Okay message is handled in handle introduce
                    IntroduceNewPartnerAddress userid port -> do
                        -- NC.sendMessage Messages.Okay hdl
                        NC.sendMessage Messages.Wait hdl
                        -- We don't know them yet, but should know them as soon as we get the message from the former comm partner
                    _ -> do
                        serial <- NSerialize.serialize deserialmessages
                        Config.traceIO $ "Error unsupported networkmessage: "++ serial
                        Config.traceIO "This is probably a timing issue! Lets resend later"
                        NC.sendMessage Messages.Wait hdl
    Config.traceNetIO $ "    Message: " ++ message

    

checkRedirectRequest :: Map.Map String (NetworkConnection Value) -> String -> IO Bool
checkRedirectRequest ncmap userid = do
    case Map.lookup userid ncmap of
        Nothing -> do
            return False
        Just networkconnection -> do
            constate <- MVar.readMVar $ ncConnectionState networkconnection
            case constate of
                RedirectRequest {} -> return True
                _ -> return False

sendRedirect ::  Handle -> Map.Map String (NetworkConnection Value) -> String -> IO ()
sendRedirect handle ncmap userid = do
    case Map.lookup userid ncmap of
        Nothing -> return ()
        Just networkconnection -> do
            constate <- MVar.readMVar $ ncConnectionState networkconnection
            case constate of
                RedirectRequest _ _ host port -> do 
                    Config.traceNetIO $ "Send redirect to:" ++ host ++ ":" ++ port
                    NC.sendMessage (Messages.Redirect host port) handle
                _ -> return ()

handleNewValue :: MVar.MVar (Map.Map String (NetworkConnection Value)) -> String -> Int -> Value -> String -> Handle -> IO ()
handleNewValue mvar userid count val ownport hdl = do
    -- networkconnectionmap <- MVar.takeMVar mvar
    networkconnectionmap <- MVar.readMVar mvar
    Config.traceNetIO $ show ownport ++ " Entered NewValue handler"
    case Map.lookup userid networkconnectionmap of
        Just networkconnection -> do
            Config.traceNetIO $ show ownport ++ " Reading message"
            success <- ND.writeMessageIfNext (ncRead networkconnection) count val
            if success then Config.traceNetIO $ show ownport ++ " Message valid" else Config.traceNetIO $ show ownport ++ " Message invalid"
            unless success $ NC.sendNetworkMessage networkconnection (Messages.RequestSync $ Data.Maybe.fromMaybe "" (ncOwnUserID networkconnection)) (-1)
            Config.traceNetIO $ show ownport ++ " Contacting peers"
            contactNewPeers val ownport
            Config.traceNetIO $ show ownport ++ " Contacted peers"
            NC.sendMessage Messages.Okay hdl
        Nothing -> do
            NC.sendMessage Messages.Okay hdl
            Config.traceNetIO "Error during recieving a networkmessage: Introduction is needed prior to sending values!"
    Config.traceNetIO $ show ownport ++ " Leaving NewValue handler"
    -- MVar.putMVar mvar networkconnectionmap

contactNewPeers :: Value -> String -> IO ()
contactNewPeers input ownport = case input of
    VSend v -> do
        nv <- contactNewPeers v ownport
        -- return $ VSend nv
        return ()
    VPair v1 v2 -> do
        nv1 <- contactNewPeers v1 ownport
        nv2 <- contactNewPeers v2 ownport
        -- return $ VPair nv1 nv2
        return () 
    VFunc penv a b -> do
        newpenv <- contactNewPeersPEnv penv ownport
        -- return $ VFunc newpenv a b
        return ()
    VDynCast v g -> do
        nv <- contactNewPeers v ownport
        -- return $ VDynCast nv g
        return ()
    VFuncCast v a b -> do
        nv <- contactNewPeers v ownport
        -- return $ VFuncCast nv a b
        return ()
    VRec penv a b c d -> do
        newpenv <- contactNewPeersPEnv penv ownport
        -- return $ VRec newpenv a b c d
        return ()
    VNewNatRec penv a b c d e f g -> do
        newpenv <- contactNewPeersPEnv penv ownport
        -- return $ VNewNatRec newpenv a b c d e f g
        return ()
    VChanSerial r w p o c -> do
        let (hostname, port) = c
        tempNC <- newNetworkConnection p o hostname port
        NClient.sendNetworkMessage tempNC (Messages.IntroduceNewPartnerAddress o ownport) 5
    _ -> return () -- return input
    where
        contactNewPeersPEnv :: [(String, Value)] -> String -> IO () -- [(String, Value)]
        contactNewPeersPEnv [] _ = return () --return []
        contactNewPeersPEnv (x:xs) ownport = do
            newval <- contactNewPeers (snd x) ownport
            rest <- contactNewPeersPEnv xs ownport
            -- return $ (fst x, newval):rest
            return ()



handleIntroduceClient :: MVar.MVar (Map.Map String (NetworkConnection Value)) -> MVar.MVar [(String, Syntax.Type)] -> (Socket, SockAddr) -> Handle -> String -> String -> Syntax.Type -> IO ()
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
                -- NC.sendMessage (Introduce serverid) hdl -- Answer with own serverid
                NC.sendMessage (Messages.OkayIntroduce serverid) hdl
                repserial <- NSerialize.serialize $ Messages.OkayIntroduce serverid
                Config.traceNetIO $ "    Response to "++ userid ++ ": " ++ repserial
                -- Adds the new user to the users that can be accepted by the server
                clientlistraw <- MVar.takeMVar clientlist
                MVar.putMVar clientlist $ clientlistraw ++ [(userid, syntype)]

            _ -> do
                Config.traceIO "Error during recieving a networkmessage: only ipv4 is currently supported!"
                MVar.putMVar mvar networkconnectionmap
                NC.sendMessage Messages.Okay hdl

handleChangePartnerAddress :: MVar.MVar (Map.Map String (NetworkConnection Value)) -> String -> String -> String -> String -> IO ()
handleChangePartnerAddress mvar userid hostname port ownport = do
    networkconnectionmap <- MVar.takeMVar mvar
    case Map.lookup userid networkconnectionmap of
        Just networkconnection -> do  -- Change to current network address
            NCon.changePartnerAddress networkconnection hostname port
            -- For some reason constate doesn't seem to properly apply

            NClient.sendNetworkMessage networkconnection (Messages.IntroduceNewPartnerAddress (Data.Maybe.fromMaybe "" (ncOwnUserID networkconnection)) ownport) 5
            MVar.putMVar mvar networkconnectionmap

        Nothing -> MVar.putMVar mvar networkconnectionmap  -- Nothing needs to be done here, the connection hasn't been established yet. No need to save that

handleRequestSync :: MVar.MVar (Map.Map String (NetworkConnection Value)) -> String -> Handle -> IO ()
handleRequestSync mvar userid hdl = do
    networkconnectionmap <- MVar.readMVar mvar
    case Map.lookup userid networkconnectionmap of
        Just networkconnection -> do  -- Change to current network address
            writevals <- ND.allMessages $ ncWrite networkconnection
            NC.sendMessage (Messages.OkaySync writevals) hdl 
            -- NClient.sendNetworkMessage networkconnection (SyncIncoming (Data.Maybe.fromMaybe "" $ ncOwnUserID networkconnection) writevals) 5
        othing -> return ()

handleSyncIncoming :: MVar.MVar (Map.Map String (NetworkConnection Value)) -> String -> [Value] -> IO ()
handleSyncIncoming mvar userid values = do
    networkconnectionmap <- MVar.readMVar mvar
    case Map.lookup userid networkconnectionmap of
        Just networkconnection -> do  -- Change to current network address
            ND.syncMessages (ncRead networkconnection) values
        Nothing -> return ()

handleRequestClose :: MVar.MVar (Map.Map String (NetworkConnection Value)) -> String -> IO ()
handleRequestClose mvar userid = do
    networkconnectionmap <- MVar.takeMVar mvar
    case Map.lookup userid networkconnectionmap of
        Just networkconnection -> do
            _ <- MVar.takeMVar $ ncRecievedRequestClose networkconnection
            MVar.putMVar (ncRecievedRequestClose networkconnection) True
        Nothing -> return ()
    MVar.putMVar mvar networkconnectionmap

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

replaceVChanSerial :: MVar.MVar (Map.Map String (NetworkConnection Value)) -> Value -> IO Value
replaceVChanSerial mvar input = case input of
    VSend v -> do
        nv <- replaceVChanSerial mvar v
        return $ VSend nv
    VPair v1 v2 -> do
        nv1 <- replaceVChanSerial mvar v1
        nv2 <- replaceVChanSerial mvar v2
        return $ VPair nv1 nv2
    VFunc penv a b -> do
        newpenv <- replaceVChanSerialPEnv mvar penv
        return $ VFunc newpenv a b
    VDynCast v g -> do
        nv <- replaceVChanSerial mvar v
        return $ VDynCast nv g
    VFuncCast v a b -> do
        nv <- replaceVChanSerial mvar v
        return $ VFuncCast nv a b
    VRec penv a b c d -> do
        newpenv <- replaceVChanSerialPEnv mvar penv
        return $ VRec newpenv a b c d
    VNewNatRec penv a b c d e f g -> do
        newpenv <- replaceVChanSerialPEnv mvar penv
        return $ VNewNatRec newpenv a b c d e f g
    VChanSerial r w p o c -> do
        networkconnection <- createNetworkConnectionS r w p o c
        ncmap <- MVar.takeMVar mvar
        MVar.putMVar mvar $ Map.insert p networkconnection ncmap
        NClient.sendNetworkMessage networkconnection (RequestSync o) 5
        used<- MVar.newEmptyMVar
        MVar.putMVar used False
        return $ VChan networkconnection mvar used
    _ -> return input
    where
        replaceVChanSerialPEnv :: MVar.MVar (Map.Map String (NetworkConnection Value)) -> [(String, Value)] -> IO [(String, Value)]
        replaceVChanSerialPEnv mvar [] = return []
        replaceVChanSerialPEnv mvar (x:xs) = do
            newval <- replaceVChanSerial mvar $ snd x
            rest <- replaceVChanSerialPEnv mvar xs
            return $ (fst x, newval):rest

ensureSocket :: Int -> MVar.MVar (Map.Map Int ServerSocket) -> IO ServerSocket
ensureSocket port socketsmvar = do
    sockets <- MVar.takeMVar socketsmvar
    case Map.lookup port sockets of
        Just socket -> do
            MVar.putMVar socketsmvar sockets
            return socket
        Nothing -> do
            Config.traceIO "Creating socket!"
            (mvar, clientlist) <- createServer port
            Config.traceIO "Socket created"
            let newsocket = (mvar, clientlist, show port)
            let updatedMap = Map.insert port newsocket sockets
            MVar.putMVar socketsmvar updatedMap
            return newsocket