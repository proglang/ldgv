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
import qualified Control.Concurrent as MVar
import Networking.NetworkConnection (newNetworkConnection, NetworkConnection (ncConnectionState, ncOwnUserID, ncRecievedRequestClose))
import Networking.Messages (Messages(Introduce, RequestSync, SyncIncoming))
import qualified Control.Concurrent as MVar
import qualified Control.Concurrent as MVar
import qualified Networking.Common as NC

createServer :: Int -> IO (MVar.MVar (Map.Map String (NetworkConnection Value)), MVar.MVar [(String, Syntax.Type)])
createServer port = do
    serverid <- UserID.newRandomUserID
    sock <- liftIO $ socket AF_INET Stream 0
    liftIO $ setSocketOption sock ReuseAddr 1
    let hints = defaultHints {
            addrFlags = [AI_PASSIVE]
          , addrSocketType = Stream
    }
    addrInfo <- liftIO $ getAddrInfo (Just hints) Nothing $ Just $ show port

    liftIO $ bind sock $ addrAddress $ head addrInfo
    liftIO $ listen sock 2
    mvar <- MVar.newEmptyMVar
    MVar.putMVar mvar Map.empty
    -- chan <- Chan.newChan
    clientlist <- MVar.newEmptyMVar
    MVar.putMVar clientlist []
    forkIO $ acceptClients mvar clientlist sock
    return (mvar, clientlist)

acceptClients :: MVar.MVar (Map.Map String (NetworkConnection Value)) -> MVar.MVar [(String, Syntax.Type)] -> Socket -> IO ()
acceptClients mvar clientlist socket = do
    putStrLn "Waiting for clients"
    clientsocket <- accept socket
    putStrLn "Accepted new client"

    forkIO $ acceptClient mvar clientlist clientsocket
    acceptClients mvar clientlist socket


-- In the nothing case we shoud wait a few seconds for other messages to resolve the issue
acceptClient :: MVar.MVar (Map.Map String (NetworkConnection Value)) -> MVar.MVar [(String, Syntax.Type)] -> (Socket, SockAddr) -> IO ()
acceptClient mvar clientlist clientsocket = do
    hdl <- NC.getHandle $ fst clientsocket
    message <- hGetLine hdl
    putStrLn $ "Recieved message:" ++ message
    case VT.runAlex message VG.parseMessages of
    -- case VT.runAlex message VG.parseValues of
        Left err -> putStrLn $ "Error during recieving a networkmessage: "++err
        Right deserialmessages -> do 
            let userid = getPartnerID deserialmessages
            netcon <- MVar.takeMVar mvar
            redirectRequest <- checkRedirectRequest netcon userid
            MVar.putMVar mvar netcon
            if redirectRequest then sendRedirect hdl netcon userid else do 
                case deserialmessages of
                    NewValue userid val -> do
                        handleNewValue mvar userid val
                    IntroduceClient userid clientport syntype-> do
                        handleIntroduceClient mvar clientlist clientsocket hdl userid clientport syntype
                    ChangePartnerAddress userid hostname port -> do
                        handleChangePartnerAddress mvar userid hostname port
                    RequestSync userid -> do
                        handleRequestSync mvar userid
                    SyncIncoming userid values -> do
                        handleSyncIncoming mvar userid values
                    RequestClose userid -> do
                        handleRequestClose mvar userid
                    _ -> do
                        serial <- NSerialize.serialize deserialmessages
                        putStrLn $ "Error unsupported networkmessage: "++ serial
                NC.sendMessage Messages.Okay hdl
    hClose hdl

checkRedirectRequest :: Map.Map String (NetworkConnection Value) -> String -> IO Bool
checkRedirectRequest ncmap userid = case Map.lookup userid ncmap of
    Nothing -> return False
    Just networkconnection -> do
        constate <- MVar.readMVar $ ncConnectionState networkconnection 
        case constate of
            RedirectRequest _ _ -> return True
            _ -> return False


sendRedirect ::  Handle -> Map.Map String (NetworkConnection Value) -> String -> IO ()
sendRedirect handle ncmap userid = case Map.lookup userid ncmap of
    Nothing -> return ()
    Just networkconnection -> do
        constate <- MVar.readMVar $ ncConnectionState networkconnection 
        case constate of
            RedirectRequest host port -> NC.sendMessage (Messages.Redirect host port) handle
            _ -> return ()



handleNewValue :: MVar.MVar (Map.Map String (NetworkConnection Value)) -> String -> Value -> IO ()
handleNewValue mvar userid val = do
    networkconnectionmap <- MVar.takeMVar mvar
    case Map.lookup userid networkconnectionmap of
        Just networkconnection -> do
            ND.writeMessage (ncRead networkconnection) val
        Nothing -> do
            putStrLn "Error during recieving a networkmessage: Introduction is needed prior to sending values!"
    MVar.putMVar mvar networkconnectionmap

handleIntroduceClient :: MVar.MVar (Map.Map String (NetworkConnection Value)) -> MVar.MVar [(String, Syntax.Type)] -> (Socket, SockAddr) -> Handle -> String -> String -> Syntax.Type -> IO ()
handleIntroduceClient mvar clientlist clientsocket hdl userid clientport syntype = do
    networkconnectionmap <- MVar.takeMVar mvar
    case Map.lookup userid networkconnectionmap of
        Just networkconnection -> do
            putStrLn "Error during recieving a networkmessage: Already introduced to this client!"
            MVar.putMVar mvar networkconnectionmap
        Nothing ->  case snd clientsocket of -- This client is new
            SockAddrInet port hostname -> do
                serverid <- UserID.newRandomUserID
                networkconnection <- newNetworkConnection userid serverid (hostaddressTypeToString hostname) clientport
                let newnetworkconnectionmap = Map.insert userid networkconnection networkconnectionmap
                MVar.putMVar mvar newnetworkconnectionmap
                NC.sendMessage (Introduce serverid) hdl -- Answer with own serverid
                -- Adds the new user to the users that can be accepted by the server
                clientlistraw <- MVar.takeMVar clientlist
                MVar.putMVar clientlist $ clientlistraw ++ [(userid, syntype)]

            _ -> do
                putStrLn "Error during recieving a networkmessage: only ipv4 is currently supported!"
                MVar.putMVar mvar networkconnectionmap

handleChangePartnerAddress :: MVar.MVar (Map.Map String (NetworkConnection Value)) -> String -> String -> String -> IO ()
handleChangePartnerAddress mvar userid hostname port = do
    networkconnectionmap <- MVar.takeMVar mvar
    case Map.lookup userid networkconnectionmap of
        Just networkconnection -> do  -- Change to current network address
            let constate = ncConnectionState networkconnection
            _ <- MVar.takeMVar constate
            MVar.putMVar constate $ Networking.NetworkConnection.Connected hostname port
            MVar.putMVar mvar networkconnectionmap

            -- Sync and request sync
            NClient.sendNetworkMessage networkconnection (RequestSync $ Data.Maybe.fromMaybe "" $ ncOwnUserID networkconnection)
            writevals <- ND.allMessages $ ncWrite networkconnection
            NClient.sendNetworkMessage networkconnection (SyncIncoming (Data.Maybe.fromMaybe "" $ ncOwnUserID networkconnection) writevals)

        Nothing -> MVar.putMVar mvar networkconnectionmap  -- Nothing needs to be done here, the connection hasn't been established yet. No need to save that

handleRequestSync :: MVar.MVar (Map.Map String (NetworkConnection Value)) -> String -> IO ()
handleRequestSync mvar userid = do
    networkconnectionmap <- MVar.readMVar mvar
    case Map.lookup userid networkconnectionmap of
        Just networkconnection -> do  -- Change to current network address
            writevals <- ND.allMessages $ ncWrite networkconnection
            NClient.sendNetworkMessage networkconnection (SyncIncoming (Data.Maybe.fromMaybe "" $ ncOwnUserID networkconnection) writevals)
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

waitForIntroduction :: Handle -> String -> IO String
waitForIntroduction handle serverid = do
    message <- hGetLine handle
    case VT.runAlex message VG.parseMessages of
        Left err -> do
            putStrLn $ "Error during client introduction: "++err
            throw $ NC.NoIntroductionException message
        Right deserial -> case deserial of
            Introduce partner -> do
                NC.sendMessage (Messages.Introduce serverid) handle
                return partner
            _ -> do
                putStrLn $ "Error during client introduction, wrong message: "++ message
                throw $ NC.NoIntroductionException message

findFittingClientMaybe :: MVar.MVar [(String, Syntax.Type)] -> Syntax.Type -> IO (Maybe String)
findFittingClientMaybe clientlist desiredType = do
    clientlistraw <- MVar.takeMVar clientlist
    let newclientlistrawAndReturn = fFCMRaw clientlistraw desiredType
    -- putStrLn "findFittingClientMaybe:"
    -- print clientlistraw
    -- putStrLn $ "Desired Type: " ++ show desiredType
    -- For some reason these prints are needed for it to work. Probably some timing thing
    -- Also we send the name of the type but not the type itself, this needs to change
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