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
import Networking.NetworkConnection (NetworkConnection(ncConnectionState, ncOwnUserID, ncRecievedRequestClose), ConnectionState (Disconnected))
import qualified Networking.Messages as Messages
import qualified Control.Concurrent as MVar
import Control.Exception
import GHC.Exception
import qualified Syntax
import qualified Networking.NetworkConnection as NCon
import qualified Networking.Common as NC
import Networking.Messages (Messages(RequestClose))
import qualified Control.Concurrent as MVar
import qualified Control.Concurrent as MVar

sendMessage :: NetworkConnection Value -> Value -> IO ()
sendMessage networkconnection val = do
    connectionstate <- MVar.takeMVar $ ncConnectionState networkconnection
    case connectionstate of
        NCon.Connected hostname port -> do
          catch (tryToSend networkconnection hostname port val) $ printConErr hostname port
        NCon.Disconnected -> putStrLn "Error when sending message: This channel is disconnected"
        NCon.Emulated -> DC.writeMessage (ncWrite networkconnection) val
    MVar.putMVar (ncConnectionState networkconnection) connectionstate


tryToSend :: NetworkConnection Value -> String -> String -> Value -> IO ()
tryToSend networkconnection hostname port val = do
    let hints = defaultHints {
            addrFlags = []
            , addrSocketType = Stream
        }
    putStrLn $ "Trying to connect to: " ++ hostname ++":"++port
    addrInfo <- getAddrInfo (Just hints) (Just hostname) $ Just port
    --addrInfo <- getAddrInfo (Just hints) (Just "127.0.0.1") $ Just port  -- Thia is obviously only for testing
    clientsocket <- NC.openSocketNC $ head addrInfo
    -- putStrLn "Before connect"
    connect clientsocket $ addrAddress $ head addrInfo
    -- putStrLn "After connect"
    handle <- NC.getHandle clientsocket
    putStrLn "Client connected: Sending Message"
    -- valcleaned <- makeVChanSendable hostname port val -- This sends a ChangeNetworkPartner Message if appropriate
    valcleaned <- NC.replaceVChan val
    NC.sendMessage (Messages.NewValue (Data.Maybe.fromMaybe "" $ ncOwnUserID networkconnection) valcleaned) handle
    DC.writeMessage (ncWrite networkconnection) valcleaned
    -- putStrLn "Sending message to old communication partner"
    sendVChanMessages hostname port val -- This sends a ChangeNetworkPartner Message if appropriate

    -- putStrLn "Disabling Chans"
    disableVChans val -- Disables all sent VChans for the sending party
    -- putStrLn "Chans disabled"
    putStrLn "Waiting for response"
    mbyresponse <- NC.recieveResponse handle
    hClose handle
    case mbyresponse of
        Just response -> case response of
            Okay -> putStrLn "Message okay"
            Redirect host port -> do 
                putStrLn "Communication partner changed address, resending"
                tryToSend networkconnection host port val
        Nothing -> putStrLn "Error when recieving response"



sendNetworkMessage :: NetworkConnection Value -> Messages -> IO ()
sendNetworkMessage networkconnection message = do
    connectionstate <- MVar.takeMVar $ ncConnectionState networkconnection
    case connectionstate of
        NCon.Connected hostname port -> do
            catch ( tryToSendNetworkMessage networkconnection hostname port message ) $ printConErr hostname port
        NCon.Disconnected -> putStrLn "Error when sending message: This channel is disconnected"
        NCon.Emulated -> pure ()
    MVar.putMVar (ncConnectionState networkconnection) connectionstate

tryToSendNetworkMessage :: NetworkConnection Value -> String -> String -> Messages -> IO ()
tryToSendNetworkMessage networkconnection hostname port message = do
    let hints = defaultHints {
                addrFlags = []
              , addrSocketType = Stream
            }
    putStrLn $ "Trying to connect to: " ++ hostname ++":"++port
    addrInfo <- getAddrInfo (Just hints) (Just hostname) $ Just port
    --addrInfo <- getAddrInfo (Just hints) (Just "127.0.0.1") $ Just port  -- Thia is obviously only for testing
    clientsocket <- NC.openSocketNC $ head addrInfo
    putStrLn "Before connect"
    connect clientsocket $ addrAddress $ head addrInfo
    putStrLn "After connect"
    handle <- NC.getHandle clientsocket
    putStrLn "Client connected: Sending NetworkMessage"
    NC.sendMessage message handle
    
    putStrLn "Waiting for response"
    mbyresponse <- NC.recieveResponse handle
    hClose handle
    case mbyresponse of
        Just response -> case response of
            Okay -> putStrLn "Message okay"
            Redirect host port -> do 
                putStrLn "Communication partner changed address, resending"
                tryToSendNetworkMessage networkconnection host port message
        Nothing -> putStrLn "Error when recieving response"

printConErr :: String -> String -> IOException -> IO ()
printConErr hostname port err = putStrLn $ "Communication Partner " ++ hostname ++ ":" ++ port ++ "not found!"


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
    putStrLn "Client connected: Introducing"
    NC.sendMessage (Messages.IntroduceClient ownuserid ownport syntype) handle
    introductionanswer <- NC.waitForServerIntroduction handle
    putStrLn "Finished Handshake"
    hClose handle
            
    newConnection <- newNetworkConnection introductionanswer ownuserid hostname port
    networkconnectionmap <- MVar.takeMVar mvar 
    let newNetworkconnectionmap = Map.insert introductionanswer newConnection networkconnectionmap
    MVar.putMVar mvar newNetworkconnectionmap
    return $ VChan newConnection

-- openSocket addr = socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)

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
    VChan nc -> do 
        putStrLn "Attempting to sending ChangePartnerAddress"
        -- connectionstate <- MVar.readMVar $ ncConnectionState nc
        -- putStrLn "Aquired connectionstate"
        sendNetworkMessage nc (Messages.ChangePartnerAddress (Data.Maybe.fromMaybe "" $ ncOwnUserID nc) newhost newport)
        -- MVar.putMVar (ncConnectionState nc) Disconnected
        -- _ <- MVar.takeMVar $ ncConnectionState nc
        -- MVar.putMVar (ncConnectionState nc) Disconnected
        putStrLn "Sent ChangePartnerAddress"
        _ <- MVar.takeMVar $ ncConnectionState nc
        putStrLn "Got connectionstate - Changeing to redirect"
        MVar.putMVar (ncConnectionState nc) $ NCon.RedirectRequest newhost newport
        putStrLn "Set RedirectRequest"
    _ -> return ()
    where
        sendVChanMessagesPEnv :: String -> String -> [(String, Value)] -> IO ()
        sendVChanMessagesPEnv _ _ [] = return ()
        sendVChanMessagesPEnv newhost newport (x:xs) = do 
            sendVChanMessages newhost newport $ snd x
            sendVChanMessagesPEnv newhost newport xs


closeConnection :: NetworkConnection Value -> IO ()
closeConnection networkconnection = do
    connectionstate <- MVar.readMVar $ ncConnectionState networkconnection
    case connectionstate of
        NCon.Connected hostname port -> do
            -- catch ( tryToSendNetworkMessage networkconnection hostname port (RequestClose $ Data.Maybe.fromMaybe "" $ ncOwnUserID networkconnection) ) $ printConErr hostname port
            waitForAck networkconnection hostname port
        NCon.Disconnected -> putStrLn "Error when sending message: This channel is disconnected"
        NCon.Emulated -> pure ()
    where
        waitForAck con hostname port = do
            connectionError <- MVar.newEmptyMVar
            MVar.putMVar connectionError False
            catch ( tryToSendNetworkMessage networkconnection hostname port (RequestClose $ Data.Maybe.fromMaybe "" $ ncOwnUserID networkconnection) ) (\exception -> do 
                printConErr hostname port exception 
                _ <- MVar.takeMVar connectionError -- If we cannot communicate with them just close the connection
                MVar.putMVar connectionError True
                )
            errorOccured <- MVar.readMVar connectionError
            if errorOccured then return () else do
                shouldClose <- MVar.readMVar $ ncRecievedRequestClose con
                if shouldClose then do 
                    putStrLn "Closing handshake completed"
                    return () 
                else do
                    MVar.threadDelay 1000000
                    waitForAck con hostname port



{-
makeVChanSendable :: String -> String -> Value -> IO Value
makeVChanSendable newhost newport input = case input of
    VSend v -> do
        nv <- makeVChanSendable newhost newport v
        return $ VSend nv
    VPair v1 v2 -> do 
        nv1 <- makeVChanSendable newhost newport v1
        nv2 <- makeVChanSendable newhost newport v2
        return $ VPair nv1 nv2
    VFunc penv a b -> do
        newpenv <- makeVChanSendablePEnv newhost newport penv
        return $ VFunc newpenv a b
    VDynCast v g -> do 
        nv <- makeVChanSendable newhost newport v
        return $ VDynCast nv g
    VFuncCast v a b -> do 
        nv <- makeVChanSendable newhost newport v
        return $ VFuncCast nv a b
    VRec penv a b c d -> do 
        newpenv <- makeVChanSendablePEnv newhost newport penv
        return $ VRec newpenv a b c d
    VNewNatRec penv a b c d e f g -> do 
        newpenv <- makeVChanSendablePEnv newhost newport penv
        return $ VNewNatRec newpenv a b c d e f g
    VChan nc -> do 
        putStrLn "Attempting to sending ChangePartnerAddress"
        -- connectionstate <- MVar.readMVar $ ncConnectionState nc
        -- putStrLn "Aquired connectionstate"
        sendNetworkMessage nc (Messages.ChangePartnerAddress (Data.Maybe.fromMaybe "" $ ncOwnUserID nc) newhost newport)
        -- MVar.putMVar (ncConnectionState nc) Disconnected
        -- _ <- MVar.takeMVar $ ncConnectionState nc
        -- MVar.putMVar (ncConnectionState nc) Disconnected
        putStrLn "Sent ChangePartnerAddress"
        return $ VChan nc
    _ -> return input
    where
        makeVChanSendablePEnv :: String -> String -> [(String, Value)] -> IO [(String, Value)]
        makeVChanSendablePEnv _ _ [] = return []
        makeVChanSendablePEnv newhost newport (x:xs) = do 
            newval <- makeVChanSendable newhost newport $ snd x
            rest <- makeVChanSendablePEnv newhost newport xs
            return $ (fst x, newval):rest
-}