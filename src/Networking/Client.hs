{-# LANGUAGE LambdaCase #-}

module Networking.Client where

import qualified Config
import Networking.NetworkConnection as NCon
import ProcessEnvironmentTypes
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
import Control.Monad
import qualified Networking.NetworkingMethod.NetworkingMethodCommon as NMC


newtype ClientException = NoIntroductionException String
    deriving Eq

instance Show ClientException where
    show = \case
        NoIntroductionException s -> "Partner didn't introduce itself, but sent: " ++ s

instance Exception ClientException


sendValue :: NMC.ActiveConnections -> NetworkConnection Value -> Value -> Int -> IO ()
sendValue activeCons networkconnection val resendOnError = do
    connectionstate <- MVar.readMVar $ ncConnectionState networkconnection
    case connectionstate of
        NCon.Connected hostname port -> do
            sendVChanMessages hostname port val
            valcleaned <- replaceVChan val
            DC.writeMessage (ncWrite networkconnection) valcleaned
            messagesCount <- DC.countMessages $ ncWrite networkconnection
            -- catch (tryToSend networkconnection hostname port val valcleaned) $ printConErr hostname port
            catch (do
                tryToSendNetworkMessage activeCons networkconnection hostname port (Messages.NewValue (Data.Maybe.fromMaybe "" $ ncOwnUserID networkconnection) messagesCount valcleaned) resendOnError
                disableVChans val
                ) $ printConErr hostname port
        NCon.Emulated -> DC.writeMessage (ncWrite networkconnection) val
        _ -> Config.traceNetIO "Error when sending message: This channel is disconnected"
    -- MVar.putMVar (ncConnectionState networkconnection) connectionstate

sendNetworkMessage :: NMC.ActiveConnections -> NetworkConnection Value -> Messages -> Int -> IO ()
sendNetworkMessage activeCons networkconnection message resendOnError = do
    connectionstate <- MVar.readMVar $ ncConnectionState networkconnection
    case connectionstate of
        NCon.Connected hostname port -> do
            catch ( tryToSendNetworkMessage activeCons networkconnection hostname port message resendOnError) $ printConErr hostname port
        NCon.Emulated -> pure ()
        _ -> Config.traceNetIO "Error when sending message: This channel is disconnected"
    --MVar.putMVar (ncConnectionState networkconnection) connectionstate

tryToSendNetworkMessage :: NMC.ActiveConnections -> NetworkConnection Value -> String -> String -> Messages -> Int -> IO ()
tryToSendNetworkMessage activeCons networkconnection hostname port message resendOnError = do
    serializedMessage <- NSerialize.serialize message
    Config.traceNetIO $ "Sending message as: " ++ Data.Maybe.fromMaybe "" (ncOwnUserID networkconnection) ++ " to: " ++  Data.Maybe.fromMaybe "" (ncPartnerUserID networkconnection)
    Config.traceNetIO $ "    Over: " ++ hostname ++ ":" ++ port
    Config.traceNetIO $ "    Message: " ++ serializedMessage
    {-
    let hints = defaultHints {
                addrFamily = AF_INET
              , addrFlags = []
              , addrSocketType = Stream
            }
    connectionsuccessful <- MVar.newEmptyMVar
    MVar.putMVar connectionsuccessful False
    response <- MVar.newEmptyMVar 
    threadid <- forkIO (do 
        Config.traceNetIO $ "Trying to connect to: " ++ hostname ++":"++port
        addrInfo <- getAddrInfo (Just hints) (Just hostname) $ Just port
        -- Config.traceNetIO "Trying to open socket"
        clientsocket <- NC.openSocketNC $ head addrInfo
        -- Config.traceNetIO "Trying to connect"
        -- This sometimes fails
        connect clientsocket $ addrAddress $ head addrInfo
        _ <- MVar.takeMVar connectionsuccessful
        MVar.putMVar connectionsuccessful True
        -- Config.traceNetIO "Connected"
        handle <- NC.getHandle clientsocket
        -- Config.traceNetIO "Trying to send!"
        NC.sendMessage message handle

        -- Config.traceNetIO "Waiting for response"
        mbyresponse <- recieveResponse handle
        hClose handle
        MVar.putMVar response mbyresponse
        )
    mbyresponse <- getResp threadid connectionsuccessful response 10
    -}


    mbycon <- NC.startConversation activeCons hostname port 10000 100
    mbyresponse <- case mbycon of
        Just con -> do
            NC.sendMessage con message
            potentialResponse <- NC.recieveResponse con 10000 100
            NC.endConversation con 10000 10
            return potentialResponse
        Nothing -> return Nothing

    case mbyresponse of
        Just response -> case response of
            Okay -> Config.traceNetIO $ "Message okay: "++serializedMessage
            OkaySync history -> do
                Config.traceNetIO $ "Message okay: "++serializedMessage
                serializedResponse <- NSerialize.serialize response
                Config.traceNetIO $ "Got syncronization values: "++serializedResponse
                DC.syncMessages (ncRead networkconnection) history
            Redirect host port -> do
                Config.traceNetIO "Communication partner changed address, resending"
                NCon.changePartnerAddress networkconnection host port
                tryToSendNetworkMessage activeCons networkconnection host port message resendOnError
            Wait -> do
                Config.traceNetIO "Communication out of sync lets wait!"
                threadDelay 1000000
                tryToSendNetworkMessage activeCons networkconnection hostname port message resendOnError
            _ -> Config.traceNetIO "Unknown communication error"

        Nothing -> do
            Config.traceNetIO "Error when recieving response"
            connectionstate <- MVar.readMVar $ ncConnectionState networkconnection
            -- connectedToPeer <- MVar.readMVar connectionsuccessful
            when (Data.Maybe.isNothing mbycon) $ Config.traceNetIO "Not connected to peer"
            Config.traceNetIO $ "Original message: " ++ serializedMessage
            case connectionstate of
                NCon.Connected newhostname newport -> if resendOnError /= 0 then do
                        Config.traceNetIO $ "Old communication partner offline! New communication partner: " ++ newhostname ++ ":" ++ newport
                        threadDelay 1000000
                        tryToSendNetworkMessage activeCons networkconnection newhostname newport message $ max (resendOnError-1) (-1)
                        else Config.traceNetIO "Old communication partner offline! No longer retrying"

                _ -> Config.traceNetIO "Error when sending message: This channel is disconnected while sending"
{-    
    where
        getResp :: ThreadId -> MVar.MVar Bool -> MVar.MVar (Maybe Responses) -> Int -> IO (Maybe Responses)
        getResp threadid connectedmvar mbyResponse count = do
            res <- tryTakeMVar mbyResponse
            case res of
                Just response -> return response
                Nothing -> if count /= 0 then do
                    threadDelay 100000
                    connected <- MVar.readMVar connectedmvar
                    if connected then getResp threadid connectedmvar mbyResponse $ max (count-1) (-1) else do
                        killThread threadid
                        return Nothing
                    else do 
                        killThread threadid
                        return Nothing
-}

printConErr :: String -> String -> IOException -> IO ()
printConErr hostname port err = Config.traceIO $ "Communication Partner " ++ hostname ++ ":" ++ port ++ "not found!"


initialConnect :: NMC.ActiveConnections -> MVar.MVar (Map.Map String (NetworkConnection Value)) -> String -> String -> String -> Syntax.Type -> IO Value
initialConnect activeCons mvar hostname port ownport syntype= do
    -- handle <- getClientHandle hostname port
    mbycon <- NC.waitForConversation activeCons hostname port 10000 100

    case mbycon of
        Just con -> do
            ownuserid <- UserID.newRandomUserID
            Config.traceIO "Client connected: Introducing"
            NC.sendMessage con (Messages.IntroduceClient ownuserid ownport syntype)
            mbyintroductionanswer <- NC.recieveResponse con 10000 (-1)
            NC.endConversation con 10000 10
            case mbyintroductionanswer of
                Just introduction -> case introduction of
                    OkayIntroduce introductionanswer -> do
                        Config.traceIO "Finished Handshake"
                        msgserial <- NSerialize.serialize $ Messages.IntroduceClient ownuserid ownport syntype
                        Config.traceNetIO $ "Sending message as: " ++ ownuserid ++ " to: " ++  introductionanswer
                        Config.traceNetIO $ "    Over: " ++ hostname ++ ":" ++ port
                        Config.traceNetIO $ "    Message: " ++ msgserial
                        newConnection <- newNetworkConnection introductionanswer ownuserid hostname port
                        networkconnectionmap <- MVar.takeMVar mvar
                        let newNetworkconnectionmap = Map.insert introductionanswer newConnection networkconnectionmap
                        MVar.putMVar mvar newNetworkconnectionmap
                        used <- MVar.newEmptyMVar
                        MVar.putMVar used False
                        return $ VChan newConnection mvar used

                    _ -> do 
                        introductionserial <- NSerialize.serialize introduction
                        Config.traceNetIO $ "Illegal answer from server: " ++ introductionserial
                        threadDelay 1000000
                        initialConnect activeCons mvar hostname port ownport syntype
                Nothing -> do 
                    Config.traceNetIO "Something went wrong while connection to the server"
                    threadDelay 1000000
                    initialConnect activeCons mvar hostname port ownport syntype
            -- hClose handle
        Nothing -> do
            Config.traceNetIO "Couldn't connect to server. Retrying"
            threadDelay 1000000
            initialConnect activeCons mvar hostname port ownport syntype


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
        {-
        sendNetworkMessage nc (Messages.ChangePartnerAddress (Data.Maybe.fromMaybe "" $ ncOwnUserID nc) newhost newport)
        _ <- MVar.takeMVar $ ncConnectionState nc
        Config.traceNetIO $ "Set RedirectRequest for " ++ (Data.Maybe.fromMaybe "" $ ncPartnerUserID nc) ++ " to " ++ newhost ++ ":" ++ newport
        MVar.putMVar (ncConnectionState nc) $ NCon.RedirectRequest newhost newport-}


        oldconnectionstate <- MVar.takeMVar $ ncConnectionState nc
        MVar.putMVar (ncConnectionState nc) $ NCon.RedirectRequest (NCon.csHostname oldconnectionstate) (NCon.csPort oldconnectionstate) newhost newport
        -- tempnetcon <- NCon.newNetworkConnectionAllowingMaybe (NCon.ncPartnerUserID nc) (NCon.ncOwnUserID nc) (NCon.csHostname oldconnectionstate) (NCon.csPort oldconnectionstate)
        -- sendNetworkMessage tempnetcon (Messages.ChangePartnerAddress (Data.Maybe.fromMaybe "" $ ncOwnUserID nc) newhost newport) 5
        Config.traceNetIO $ "Set RedirectRequest for " ++ (Data.Maybe.fromMaybe "" $ ncPartnerUserID nc) ++ " to " ++ newhost ++ ":" ++ newport
    _ -> return ()
    where
        sendVChanMessagesPEnv :: String -> String -> [(String, Value)] -> IO ()
        sendVChanMessagesPEnv _ _ [] = return ()
        sendVChanMessagesPEnv newhost newport (x:xs) = do
            sendVChanMessages newhost newport $ snd x
            sendVChanMessagesPEnv newhost newport xs

closeConnection _ = return ()

-- Close Connection is no longer needed 
{-
closeConnection :: NetworkConnection Value -> IO ()
closeConnection con = do
    connectionstate <- MVar.readMVar $ ncConnectionState con
    case connectionstate of
        NCon.Connected hostname port -> do
            connectionError <- MVar.newEmptyMVar
            MVar.putMVar connectionError False
            catch ( tryToSendNetworkMessage con hostname port (RequestClose $ Data.Maybe.fromMaybe "" $ ncOwnUserID con) 0) (\exception -> do
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
        NCon.Emulated -> pure ()
        _ -> Config.traceIO "Error when sending message: This channel is disconnected"
-}
{-
recieveResponse :: Handle -> IO (Maybe Responses)
recieveResponse handle = do
    retVal <- MVar.newEmptyMVar 
    forkIO $ NC.recieveMessage handle VG.parseResponses (\_ -> MVar.putMVar retVal Nothing) (\_ des -> MVar.putMVar retVal $ Just des)
    waitForResponse retVal 100
    where
        waitForResponse :: MVar.MVar (Maybe Responses) -> Int -> IO (Maybe Responses)
        waitForResponse mvar count = do
            result <- MVar.tryTakeMVar mvar
            case result of
                Just mbyResponse -> do 
                    -- Config.traceNetIO "Got response"
                    return mbyResponse
                Nothing -> if count /= 0 then do
                    -- Config.traceNetIO $ "Waiting for response: " ++ show count
                    threadDelay 10000
                    waitForResponse mvar (count-1)
                    else return Nothing

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
-}
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

{-
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
-}