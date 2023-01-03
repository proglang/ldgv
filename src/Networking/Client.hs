{-# LANGUAGE LambdaCase #-}

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
import qualified Config
import qualified Networking.Serialize as NSerialize


newtype ClientException = NoIntroductionException String
    deriving Eq

instance Show ClientException where
    show = \case
        NoIntroductionException s -> "Partner didn't introduce itself, but sent: " ++ s

instance Exception ClientException


sendValue :: NetworkConnection Value -> Value -> IO ()
sendValue networkconnection val = do
    connectionstate <- MVar.readMVar $ ncConnectionState networkconnection
    case connectionstate of
        NCon.Connected hostname port -> do
            valcleaned <- replaceVChan val
            DC.writeMessage (ncWrite networkconnection) valcleaned
            -- catch (tryToSend networkconnection hostname port val valcleaned) $ printConErr hostname port
            catch (do 
                tryToSendNetworkMessage networkconnection hostname port (Messages.NewValue (Data.Maybe.fromMaybe "" $ ncOwnUserID networkconnection) valcleaned)
                sendVChanMessages hostname port val
                disableVChans val
                ) $ printConErr hostname port
        NCon.Disconnected -> Config.traceIO "Error when sending message: This channel is disconnected"
        NCon.Emulated -> DC.writeMessage (ncWrite networkconnection) val
    -- MVar.putMVar (ncConnectionState networkconnection) connectionstate

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
    serializedMessage <- NSerialize.serialize message
    Config.traceNetIO $ "Sending message as: " ++ Data.Maybe.fromMaybe "" (ncOwnUserID networkconnection) ++ " to: " ++  Data.Maybe.fromMaybe "" (ncPartnerUserID networkconnection)
    Config.traceNetIO $ "    Over: " ++ hostname ++ ":" ++ port
    Config.traceNetIO $ "    Message: " ++ serializedMessage
    let hints = defaultHints {
                addrFamily = AF_INET
              , addrFlags = []
              , addrSocketType = Stream
            }
    Config.traceIO $ "Trying to connect to: " ++ hostname ++":"++port
    addrInfo <- getAddrInfo (Just hints) (Just hostname) $ Just port
    clientsocket <- NC.openSocketNC $ head addrInfo
    connect clientsocket $ addrAddress $ head addrInfo
    handle <- NC.getHandle clientsocket
    NC.sendMessage message handle

    Config.traceIO "Waiting for response"
    mbyresponse <- recieveResponse handle
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
    handle <- getClientHandle hostname port
    ownuserid <- UserID.newRandomUserID
    Config.traceIO "Client connected: Introducing"
    NC.sendMessage (Messages.IntroduceClient ownuserid ownport syntype) handle
    introductionanswer <- waitForServerIntroduction handle
    Config.traceIO "Finished Handshake"

    msgserial <- NSerialize.serialize $ Messages.IntroduceClient ownuserid ownport syntype
    Config.traceNetIO $ "Sending message as: " ++ ownuserid ++ " to: " ++  introductionanswer
    Config.traceNetIO $ "    Over: " ++ hostname ++ ":" ++ port
    Config.traceNetIO $ "    Message: " ++ msgserial
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
        Config.traceNetIO $ "Set RedirectRequest for " ++ (Data.Maybe.fromMaybe "" $ ncPartnerUserID nc) ++ " to " ++ newhost ++ ":" ++ newport
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


recieveResponse :: Handle -> IO (Maybe Responses)
recieveResponse handle = do
    NC.recieveMessage handle VG.parseResponses (\_ -> return Nothing) (\_ des -> return $ Just des)

-- This waits until the handle is established
getClientHandle :: String -> String -> IO Handle
getClientHandle hostname port = do
    catch ( do
        let hints = defaultHints {
                addrFlags = []
              , addrSocketType = Stream
            }
        addrInfo <- getAddrInfo (Just hints) (Just hostname) $ Just port
        clientsocket <- NC.openSocketNC $ head addrInfo
        connect clientsocket $ addrAddress $ head addrInfo
        NC.getHandle clientsocket) $ expredirect hostname port
    where
        expredirect :: String -> String -> IOException -> IO Handle
        expredirect hostname port e = do
            threadDelay 1000000
            getClientHandle hostname port

replaceVChan :: Value -> IO Value
replaceVChan input = case input of
    VSend v -> do
        nv <- replaceVChan v
        return $ VSend nv
    VPair v1 v2 -> do
        nv1 <- replaceVChan v1
        nv2 <- replaceVChan v2
        return $ VPair nv1 nv2
    VFunc penv a b -> do
        newpenv <- replaceVChanPEnv penv
        return $ VFunc newpenv a b
    VDynCast v g -> do
        nv <- replaceVChan v
        return $ VDynCast nv g
    VFuncCast v a b -> do
        nv <- replaceVChan v
        return $ VFuncCast nv a b
    VRec penv a b c d -> do
        newpenv <- replaceVChanPEnv penv
        return $ VRec newpenv a b c d
    VNewNatRec penv a b c d e f g -> do
        newpenv <- replaceVChanPEnv penv
        return $ VNewNatRec newpenv a b c d e f g
    VChan nc _ _-> do
        (r, rl, w, wl, pid, oid, h, p) <- serializeNetworkConnection nc
        return $ VChanSerial (r, rl) (w, wl) pid oid (h, p)
    _ -> return input
    where
        replaceVChanPEnv :: [(String, Value)] -> IO [(String, Value)]
        replaceVChanPEnv [] = return []
        replaceVChanPEnv (x:xs) = do
            newval <- replaceVChan $ snd x
            rest <- replaceVChanPEnv xs
            return $ (fst x, newval):rest

waitForServerIntroduction :: Handle -> IO String
waitForServerIntroduction handle = do
    NC.recieveMessage handle VG.parseResponses (throw . NoIntroductionException) deserHandler
    where
        deserHandler message deserial = case deserial of
            OkayIntroduce partner -> do
                return partner
            _ -> do
                Config.traceIO $ "Error during server introduction, wrong message: "++ message
                throw $ NoIntroductionException message
