{-# LANGUAGE LambdaCase #-}
module Networking.Server where

import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.Chan as Chan
import Control.Concurrent (forkIO)
import Control.Monad.IO.Class
import qualified Data.Map as Map
import GHC.IO.Handle
import Network.Socket

import Networking.Messages
import qualified ValueParsing.ValueTokens as VT
import qualified ValueParsing.ValueGrammar as VG
import qualified Networking.Common as NC
import qualified Networking.Serialize as NSerialize
import ProcessEnvironment

import Control.Exception
import qualified Networking.UserID as UserID
import qualified Networking.Messages as Messages
import qualified Networking.DirectionalConnection as ND

import Networking.NetworkConnection
import qualified Control.Concurrent as MVar
import Networking.NetworkConnection (newNetworkConnection, NetworkConnection (ncConnectionState))
import Networking.Messages (Messages(Introduce))


createServer :: Int -> IO (MVar.MVar (Map.Map String ConnectionInfo), Chan.Chan String, String)
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
    chan <- Chan.newChan
    forkIO $ acceptClients mvar chan sock serverid
    return (mvar, chan, serverid)


createServerNew :: Int -> IO (MVar.MVar (Map.Map String (NetworkConnection Value)), Chan.Chan String)
createServerNew port = do
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
    chan <- Chan.newChan
    forkIO $ acceptClientsNew mvar chan sock
    return (mvar, chan)

acceptClientsNew :: MVar.MVar (Map.Map String (NetworkConnection Value)) -> Chan.Chan String -> Socket -> IO ()
acceptClientsNew mvar chan socket = do
    putStrLn "Waiting for clients"
    clientsocket <- accept socket
    putStrLn "Accepted new client"

    forkIO $ acceptClientNew mvar chan clientsocket
    acceptClientsNew mvar chan socket

acceptClientNew :: MVar.MVar (Map.Map String (NetworkConnection Value)) -> Chan.Chan String -> (Socket, SockAddr) -> IO ()
acceptClientNew mvar chan clientsocket = do
    hdl <- NC.getHandle $ fst clientsocket
    message <- hGetLine hdl
    putStrLn $ "Recieved message:" ++ message
    case VT.runAlex message VG.parseMessages of
    -- case VT.runAlex message VG.parseValues of
        Left err -> putStrLn $ "Error during recieving a networkmessage: "++err
        Right deserialmessages -> case deserialmessages of
            NewValue userid val -> do
                networkconnectionmap <- MVar.takeMVar mvar
                case Map.lookup userid networkconnectionmap of
                    Just networkconnection -> do  -- This means we habe already spoken to this client
                        valCleaned <- NC.replaceVChanSerial val -- Replaces VChanSerial with VChans and their appropriate connection
                        ND.writeMessage (ncRead networkconnection) valCleaned
                        MVar.putMVar mvar networkconnectionmap
                    Nothing -> do
                        putStrLn "Error during recieving a networkmessage: Introduction is needed prior to sending values!"
                        MVar.putMVar mvar networkconnectionmap
            IntroduceClient userid clientport -> do
                networkconnectionmap <- MVar.takeMVar mvar
                case Map.lookup userid networkconnectionmap of
                    Just networkconnection -> do
                        putStrLn "Error during recieving a networkmessage: Already introduced to this client!"
                        MVar.putMVar mvar networkconnectionmap
                    Nothing ->  case snd clientsocket of -- This client is new
                        SockAddrInet port hostname -> do
                            serverid <- UserID.newRandomUserID
                            networkconnection <- newNetworkConnection userid serverid (show hostname) clientport
                            let newnetworkconnectionmap = Map.insert userid networkconnection networkconnectionmap
                            MVar.putMVar mvar newnetworkconnectionmap
                            NC.sendMessage (Introduce serverid) hdl -- Answer with own serverid
                            Chan.writeChan chan userid -- Adds the new user to the users that can be accepted by the server

                        _ -> do 
                            putStrLn "Error during recieving a networkmessage: only ipv4 is currently supported!"
                            MVar.putMVar mvar networkconnectionmap

            
            ChangePartnerAddress userid hostname port -> do
                networkconnectionmap <- MVar.takeMVar mvar
                case Map.lookup userid networkconnectionmap of
                    Just networkconnection -> do  -- Change to current network address
                        let constate = ncConnectionState networkconnection
                        _ <- MVar.takeMVar constate
                        MVar.putMVar constate $ Networking.NetworkConnection.Connected hostname port
                        MVar.putMVar mvar networkconnectionmap
                    Nothing -> pure ()  -- Nothing needs to be done here, the connection hasn't been established yet. No need to save that

            _ -> do
                serial <- NSerialize.serialize deserialmessages
                putStrLn $ "Error unsupported networkmessage: "++ serial
    hClose hdl




acceptClients :: MVar.MVar (Map.Map String ConnectionInfo) -> Chan.Chan String -> Socket -> String-> IO ()
acceptClients mvar chan socket serverid = do
    putStrLn "Waiting for clients"
    clientsocket <- accept socket
    putStrLn "Accepted new client"

    forkIO $ acceptClient mvar chan clientsocket serverid
    acceptClients mvar chan socket serverid


acceptClient :: MVar.MVar (Map.Map String ConnectionInfo) -> Chan.Chan String -> (Socket, SockAddr) -> String -> IO ()
acceptClient mvar chan clientsocket serverid = do
    hdl <- NC.getHandle $ fst clientsocket
    userid <- waitForIntroduction hdl serverid
    r <- ND.newConnection
    w <- ND.newConnection
    MVar.modifyMVar_ mvar (return . Map.insert userid (ConnectionInfo hdl (snd clientsocket) r w))
    forkIO $ NC.recieveMessagesID r mvar userid
    Chan.writeChan chan userid

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