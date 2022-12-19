module Networking.Client where

import qualified Config
import Networking.NetworkConnection as NCon
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
import Networking.NetworkConnection (NetworkConnection(ncConnectionState, ncOwnUserID, ncRecievedRequestClose), ConnectionState (Disconnected))
import Control.Concurrent
import Control.Exception
import GHC.Exception
import qualified Syntax
import qualified Networking.Common as NC
import Networking.Messages (Messages(RequestClose))
import qualified Networking.NetworkConnection as NCon
import qualified Control.Concurrent as MVar

sendMessage :: NetworkConnection Value -> Value -> IO ()
sendMessage networkconnection val = do
    connectionstate <- MVar.readMVar $ ncConnectionState networkconnection
    case connectionstate of
        NCon.Connected hostname port -> do
            valcleaned <- NC.replaceVChan val
            DC.writeMessage (ncWrite networkconnection) valcleaned
            catch (tryToSend networkconnection hostname port val valcleaned) $ printConErr hostname port
        NCon.Disconnected -> Config.traceIO "Error when sending message: This channel is disconnected"
        NCon.Emulated -> DC.writeMessage (ncWrite networkconnection) val
    -- MVar.putMVar (ncConnectionState networkconnection) connectionstate


tryToSend :: NetworkConnection Value -> String -> String -> Value -> Value -> IO ()
tryToSend networkconnection hostname port val valcleaned = do
    let hints = defaultHints {
            addrFlags = []
            , addrSocketType = Stream
        }
    Config.traceIO $ "Trying to connect to: " ++ hostname ++":"++port
    addrInfo <- getAddrInfo (Just hints) (Just hostname) $ Just port
    clientsocket <- NC.openSocketNC $ head addrInfo
    connect clientsocket $ addrAddress $ head addrInfo
    handle <- NC.getHandle clientsocket

    NC.sendMessage (Messages.NewValue (Data.Maybe.fromMaybe "" $ ncOwnUserID networkconnection) valcleaned) handle
    sendVChanMessages hostname port val -- This sends a ChangeNetworkPartner Message if appropriate

    disableVChans val -- Disables all sent VChans for the sending party
    Config.traceIO "Waiting for response"
    mbyresponse <- NC.recieveResponse handle
    hClose handle
    case mbyresponse of
        Just response -> case response of
            Okay -> Config.traceIO "Message okay"
            Redirect host port -> do 
                Config.traceIO "Communication partner changed address, resending"
                NCon.changePartnerAddress networkconnection host port
                tryToSend networkconnection host port val valcleaned
        Nothing -> Config.traceIO "Error when recieving response"



sendNetworkMessage :: NetworkConnection Value -> Messages -> IO ()
sendNetworkMessage networkconnection message = do
    connectionstate <- MVar.readMVar $ ncConnectionState networkconnection
    case connectionstate of
        NCon.Connected hostname port -> do
            catch ( tryToSendNetworkMessage networkconnection hostname port message ) $ printConErr hostname port
        NCon.Disconnected -> Config.traceIO "Error when sending message: This channel is disconnected"
        NCon.Emulated -> pure ()
    --MVar.putMVar (ncConnectionState networkconnection) connectionstate

tryToSendNetworkMessage :: NetworkConnection Value -> String -> String -> Messages -> IO ()
tryToSendNetworkMessage networkconnection hostname port message = do
    let hints = defaultHints {
                addrFlags = []
              , addrSocketType = Stream
            }
    Config.traceIO $ "Trying to connect to: " ++ hostname ++":"++port
    addrInfo <- getAddrInfo (Just hints) (Just hostname) $ Just port
    clientsocket <- NC.openSocketNC $ head addrInfo
    connect clientsocket $ addrAddress $ head addrInfo
    handle <- NC.getHandle clientsocket
    NC.sendMessage message handle
    
    Config.traceIO "Waiting for response"
    mbyresponse <- NC.recieveResponse handle
    hClose handle
    case mbyresponse of
        Just response -> case response of
            Okay -> Config.traceIO "Message okay"
            Redirect host port -> do 
                Config.traceIO "Communication partner changed address, resending"
                NCon.changePartnerAddress networkconnection host port
                tryToSendNetworkMessage networkconnection host port message
        Nothing -> Config.traceIO "Error when recieving response"

printConErr :: String -> String -> IOException -> IO ()
printConErr hostname port err = Config.traceIO $ "Communication Partner " ++ hostname ++ ":" ++ port ++ "not found!"


initialConnect :: MVar.MVar (Map.Map String (NetworkConnection Value)) -> String -> String -> String -> Syntax.Type -> IO Value
initialConnect mvar hostname port ownport syntype= do
    let hints = defaultHints {
                addrFlags = []
              , addrSocketType = Stream
            }   
    addrInfo <- getAddrInfo (Just hints) (Just hostname) $ Just port
    clientsocket <- NC.openSocketNC $ head addrInfo
    connect clientsocket $ addrAddress $ head addrInfo
    handle <- NC.getHandle clientsocket
    ownuserid <- UserID.newRandomUserID
    Config.traceIO "Client connected: Introducing"
    NC.sendMessage (Messages.IntroduceClient ownuserid ownport syntype) handle
    introductionanswer <- NC.waitForServerIntroduction handle
    Config.traceIO "Finished Handshake"
    hClose handle
            
    newConnection <- newNetworkConnection introductionanswer ownuserid hostname port
    networkconnectionmap <- MVar.takeMVar mvar 
    let newNetworkconnectionmap = Map.insert introductionanswer newConnection networkconnectionmap
    MVar.putMVar mvar newNetworkconnectionmap
    used <- MVar.newEmptyMVar
    MVar.putMVar used False
    return $ VChan newConnection mvar used

sendVChanMessages :: String -> String -> Value -> IO ()
sendVChanMessages newhost newport input = case input of
    VSend v -> sendVChanMessages newhost newport v
    VPair v1 v2 -> do 
        sendVChanMessages newhost newport v1
        sendVChanMessages newhost newport v2
    VFunc penv a b -> sendVChanMessagesPEnv newhost newport penv
    VDynCast v g -> sendVChanMessages newhost newport v
    VFuncCast v a b -> sendVChanMessages newhost newport v
    VRec penv a b c d -> sendVChanMessagesPEnv newhost newport penv
    VNewNatRec penv a b c d e f g -> sendVChanMessagesPEnv newhost newport penv
    VChan nc _ _-> do 
        sendNetworkMessage nc (Messages.ChangePartnerAddress (Data.Maybe.fromMaybe "" $ ncOwnUserID nc) newhost newport)
        _ <- MVar.takeMVar $ ncConnectionState nc
        putStrLn $ "Set RedirectRequest for " ++ (Data.Maybe.fromMaybe "" $ ncPartnerUserID nc) ++ " to " ++ newhost ++ ":" ++ newport
        MVar.putMVar (ncConnectionState nc) $ NCon.RedirectRequest newhost newport
    _ -> return ()
    where
        sendVChanMessagesPEnv :: String -> String -> [(String, Value)] -> IO ()
        sendVChanMessagesPEnv _ _ [] = return ()
        sendVChanMessagesPEnv newhost newport (x:xs) = do 
            sendVChanMessages newhost newport $ snd x
            sendVChanMessagesPEnv newhost newport xs


closeConnection :: NetworkConnection Value -> IO ()
closeConnection con = do
    connectionstate <- MVar.readMVar $ ncConnectionState con
    case connectionstate of
        NCon.Connected hostname port -> do
            connectionError <- MVar.newEmptyMVar
            MVar.putMVar connectionError False
            catch ( tryToSendNetworkMessage con hostname port (RequestClose $ Data.Maybe.fromMaybe "" $ ncOwnUserID con) ) (\exception -> do 
                printConErr hostname port exception 
                _ <- MVar.takeMVar connectionError -- If we cannot communicate with them just close the connection
                MVar.putMVar connectionError True
                )
            errorOccured <- MVar.readMVar connectionError
            if errorOccured then return () else do
                shouldClose <- MVar.readMVar $ ncRecievedRequestClose con
                if shouldClose then do 
                    Config.traceIO "Closing handshake completed"
                    return () 
                else do
                    threadDelay 1000000
                    closeConnection con
        NCon.Disconnected -> Config.traceIO "Error when sending message: This channel is disconnected"
        NCon.Emulated -> pure ()
