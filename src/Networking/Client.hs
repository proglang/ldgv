module Networking.Client where

import Networking.NetworkConnection as NCon
import qualified Networking.Common as NC
import ProcessEnvironment
import qualified ValueParsing.ValueTokens as VT
import qualified ValueParsing.ValueGrammar as VG
import Networking.Messages
import qualified Control.Concurrent.MVar as MVar
import qualified Networking.DirectionalConnection as DC
import Network.Socket
import qualified Networking.Messages as Messages
import qualified Networking.NetworkConnection as Networking
import qualified Networking.UserID as UserID
import qualified Data.Map as Map
import GHC.IO.Handle
import qualified Data.Maybe

sendMessage :: NetworkConnection Value -> Value -> IO ()
sendMessage networkconnection val = do
    let hints = defaultHints {
                addrFlags = []
              , addrSocketType = Stream
            }
    connectionstate <- MVar.takeMVar $ ncConnectionState networkconnection
    case connectionstate of
        NCon.Connected hostname port -> do
            putStrLn $ "Trying to connect to: " ++ hostname ++":"++port
            addrInfo <- getAddrInfo (Just hints) (Just hostname) $ Just port
            --addrInfo <- getAddrInfo (Just hints) (Just "127.0.0.1") $ Just port  -- Thia is obviously only for testing
            clientsocket <- NC.openSocketNC $ head addrInfo
            putStrLn "Before connect"
            connect clientsocket $ addrAddress $ head addrInfo
            putStrLn "After connect"
            handle <- NC.getHandle clientsocket
            putStrLn "Client connected: Sending Message"
            NC.sendMessage (Messages.NewValue (Data.Maybe.fromMaybe "" $ ncOwnUserID networkconnection) val) handle
            DC.writeMessage (ncWrite networkconnection) val
            hClose handle
        NCon.Disconnected -> putStrLn "Error when sending message: This channel is disconnected"
        NCon.Emulated -> DC.writeMessage (ncWrite networkconnection) val
    MVar.putMVar (ncConnectionState networkconnection) connectionstate


initialConnect :: MVar.MVar (Map.Map String (NetworkConnection Value)) -> String -> String -> String -> IO Value
initialConnect mvar hostname port ownport= do
    let hints = defaultHints {
                addrFlags = []
              , addrSocketType = Stream
            }
    networkconnectionmap <- MVar.takeMVar mvar    
    addrInfo <- getAddrInfo (Just hints) (Just hostname) $ Just port
    clientsocket <- NC.openSocketNC $ head addrInfo
    connect clientsocket $ addrAddress $ head addrInfo
    handle <- NC.getHandle clientsocket
    ownuserid <- UserID.newRandomUserID
    putStrLn "Client connected: Introducing"
    NC.sendMessage (Messages.IntroduceClient ownuserid ownport) handle
    introductionanswer <- NC.waitForServerIntroduction handle
    putStrLn "Finished Handshake"
    hClose handle
            
    newConnection <- newNetworkConnection introductionanswer ownuserid hostname port
    let newNetworkconnectionmap = Map.insert introductionanswer newConnection networkconnectionmap
    MVar.putMVar mvar newNetworkconnectionmap
    return $ VChan newConnection

-- openSocket addr = socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)

