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
import Networking.NetworkConnection (NetworkConnection(ncConnectionState), ConnectionState (Disconnected))
import qualified Networking.Messages as Messages
import qualified Control.Concurrent as MVar
import qualified Control.Concurrent as MVar
import Control.Exception
import GHC.Exception

sendMessage :: NetworkConnection Value -> Value -> IO ()
sendMessage networkconnection val = do
    let hints = defaultHints {
                addrFlags = []
              , addrSocketType = Stream
            }
    connectionstate <- MVar.takeMVar $ ncConnectionState networkconnection
    case connectionstate of
        NCon.Connected hostname port -> do
          catch ( do
            putStrLn $ "Trying to connect to: " ++ hostname ++":"++port
            addrInfo <- getAddrInfo (Just hints) (Just hostname) $ Just port
            --addrInfo <- getAddrInfo (Just hints) (Just "127.0.0.1") $ Just port  -- Thia is obviously only for testing
            clientsocket <- NC.openSocketNC $ head addrInfo
            putStrLn "Before connect"
            connect clientsocket $ addrAddress $ head addrInfo
            putStrLn "After connect"
            handle <- NC.getHandle clientsocket
            putStrLn "Client connected: Sending Message"
            valcleaned <- makeVChanSendable hostname port val
            NC.sendMessage (Messages.NewValue (Data.Maybe.fromMaybe "" $ ncOwnUserID networkconnection) valcleaned) handle
            DC.writeMessage (ncWrite networkconnection) valcleaned
            putStrLn "Disabling Chans"
            disableVChans val -- Disables all sent VChans for the sending party
            putStrLn "Chans disabled"
            hClose handle ) $ printConErr hostname port
        NCon.Disconnected -> putStrLn "Error when sending message: This channel is disconnected"
        NCon.Emulated -> DC.writeMessage (ncWrite networkconnection) val
    MVar.putMVar (ncConnectionState networkconnection) connectionstate


sendNetworkMessage :: NetworkConnection Value -> Messages -> IO ()
sendNetworkMessage networkconnection message = do
    let hints = defaultHints {
                addrFlags = []
              , addrSocketType = Stream
            }
    connectionstate <- MVar.takeMVar $ ncConnectionState networkconnection
    case connectionstate of
        NCon.Connected hostname port -> do
            catch ( do
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
              hClose handle ) $ printConErr hostname port
        NCon.Disconnected -> putStrLn "Error when sending message: This channel is disconnected"
        NCon.Emulated -> pure ()
    MVar.putMVar (ncConnectionState networkconnection) connectionstate



printConErr :: String -> String -> IOException -> IO ()
printConErr hostname port err = putStrLn $ "Communication Partner " ++ hostname ++ ":" ++ port ++ "not found!"


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