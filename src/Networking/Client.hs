{-# LANGUAGE LambdaCase #-}

module Networking.Client where

import qualified Config
import ProcessEnvironmentTypes
import Networking.Messages
import qualified Control.Concurrent.MVar as MVar
import qualified Networking.NetworkBuffer as NB
import qualified Networking.Messages as Messages
import qualified Networking.RandomID as RandomID
import qualified Data.Map as Map
import Control.Concurrent
import Control.Exception
import qualified Syntax
import qualified Networking.Common as NC
import Networking.NetworkConnection
import qualified Networking.Serialize as NSerialize
import Control.Monad
import qualified Networking.NetworkingMethod.NetworkingMethodCommon as NMC
import qualified Control.Concurrent.SSem as SSem


newtype ClientException = NoIntroductionException String
    deriving Eq

instance Show ClientException where
    show = \case
        NoIntroductionException s -> "Partner didn't introduce itself, but sent: " ++ s

instance Exception ClientException

sendValue :: VChanConnections -> NMC.ActiveConnections -> NetworkConnection Value -> Value -> String -> Int -> IO Bool
sendValue vchanconsmvar activeCons networkconnection val ownport resendOnError = do
    connectionstate <- MVar.readMVar $ ncConnectionState networkconnection
    case connectionstate of
        Connected hostname port _ _ _ -> do
            --waitTillReadyToSend val
            setRedirectRequests vchanconsmvar hostname port ownport val
            valcleaned <- serializeVChan val
            messagesCount <- NB.write (ncWrite networkconnection) valcleaned
            tryToSendNetworkMessage activeCons networkconnection hostname port (Messages.NewValue (ncOwnUserID networkconnection) messagesCount valcleaned) resendOnError
        Emulated {} -> do
            --waitTillReadyToSend val
            vchancons <- MVar.readMVar vchanconsmvar
            valCleaned <- serializeVChan val
            NB.write(ncWrite networkconnection) valCleaned
            let ownid = ncOwnUserID networkconnection
            let mbypartner = Map.lookup ownid vchancons
            case mbypartner of
                Just partner -> do 
                    NB.write (ncRead partner) valCleaned
                    return True
                _ -> do 
                    Config.traceNetIO "Something went wrong when sending over a emulated connection"
                    return False
        _ -> do 
            Config.traceNetIO "Error when sending message: This channel is disconnected"
            return False

waitTillReadyToSend :: Value -> IO ()
waitTillReadyToSend input = do
    ready <- channelReadyToSend input
    unless ready $ threadDelay 5000 >> waitTillReadyToSend input

channelReadyToSend :: Value -> IO Bool
channelReadyToSend = searchVChans handleChannel True (&&)
    where
        handleChannel :: Value -> IO Bool
        handleChannel input = case input of
            VChan nc used -> NB.isAllAcknowledged $ ncWrite nc
            _ -> return True

sendNetworkMessage :: NMC.ActiveConnections -> NetworkConnection Value -> Message -> Int -> IO Bool
sendNetworkMessage activeCons networkconnection message resendOnError = do
    connectionstate <- MVar.readMVar $ ncConnectionState networkconnection
    case connectionstate of
        Connected hostname port _ _ _ -> do
            tryToSendNetworkMessage activeCons networkconnection hostname port message resendOnError
        Emulated {} -> return True
        _ -> do 
            Config.traceNetIO "Error when sending message: This channel is disconnected"
            return False

tryToSendNetworkMessage :: NMC.ActiveConnections -> NetworkConnection Value -> String -> String -> Message -> Int -> IO Bool
tryToSendNetworkMessage activeCons networkconnection hostname port message resendOnError = do
    serializedMessage <- NSerialize.serialize message
    sendingNetLog serializedMessage $ "Sending message as: " ++ ncOwnUserID networkconnection ++ " to: " ++  ncPartnerUserID networkconnection ++ " Over: " ++ hostname ++ ":" ++ port

    mbycon <- NC.startConversation activeCons hostname port 10000 10
    mbyresponse <- case mbycon of
        Just con -> do
            sendingNetLog serializedMessage "Aquired connection"
            NC.sendMessage con message
            sendingNetLog serializedMessage "Sent message"
            potentialResponse <- NC.recieveResponse con 10000 50
            sendingNetLog serializedMessage "Recieved response"
            NC.endConversation con 10000 10
            sendingNetLog serializedMessage "Ended connection"
            return potentialResponse
        Nothing -> do 
            sendingNetLog serializedMessage "Connecting unsuccessful"
            return Nothing

    success <- case mbyresponse of
        Just response -> case response of
            Okay -> do 
                sendingNetLog serializedMessage "Message okay" 
                return True
            Redirect host port -> do
                sendingNetLog serializedMessage "Communication partner changed address, resending"
                tryToSendNetworkMessage activeCons networkconnection host port message resendOnError
            Wait -> do
                sendingNetLog serializedMessage "Communication out of sync lets wait!"
                threadDelay 1000000
                tryToSendNetworkMessage activeCons networkconnection hostname port message resendOnError
            _ -> do 
                sendingNetLog serializedMessage "Unknown communication error"
                return False

        Nothing -> do
            sendingNetLog serializedMessage "Error when recieving response"
            if resendOnError /= 0 then do
                connectionState <- MVar.readMVar $ ncConnectionState networkconnection
                case connectionState of
                    Connected updatedhost updatedport _ _ _ -> do 
                        sendingNetLog serializedMessage $ "Trying to resend to: " ++ updatedhost ++ ":" ++ updatedport
                        tryToSendNetworkMessage activeCons networkconnection updatedhost updatedport message $ max (resendOnError-1) (-1)
                    _ -> return False
            else return False
    sendingNetLog serializedMessage "Message got send or finally failed!"
    return success


sendingNetLog :: String -> String -> IO ()
sendingNetLog msg info = Config.traceNetIO $ "Sending message: "++msg++" \n    Status: "++info

printConErr :: String -> String -> IOException -> IO Bool
printConErr hostname port err = do 
    Config.traceIO $ "Communication Partner " ++ hostname ++ ":" ++ port ++ "not found! \n    " ++ show err
    return False


initialConnect :: NMC.ActiveConnections -> MVar.MVar (Map.Map String (NetworkConnection Value)) -> String -> String -> String -> (Syntax.Type, Syntax.Type) -> IO Value
initialConnect activeCons mvar hostname port ownport syntype= do
    mbycon <- NC.waitForConversation activeCons hostname port 1000 100  -- This should be 10000 100 in the real world, expecting just a 100ms ping in the real world might be a little aggressive.

    case mbycon of
        Just con -> do
            ownuserid <- RandomID.newRandomID
            NC.sendMessage con (Messages.IntroduceClient ownuserid ownport (fst syntype) $ snd syntype)
            mbyintroductionanswer <- NC.recieveResponse con 10000 (-1)
            NC.endConversation con 10000 10
            case mbyintroductionanswer of
                Just introduction -> case introduction of
                    OkayIntroduce introductionanswer -> do
                        msgserial <- NSerialize.serialize $ Messages.IntroduceClient ownuserid ownport (fst syntype) $ snd syntype
                        Config.traceNetIO $ "Sending message as: " ++ ownuserid ++ " to: " ++  introductionanswer
                        Config.traceNetIO $ "    Over: " ++ hostname ++ ":" ++ port
                        Config.traceNetIO $ "    Message: " ++ msgserial
                        newConnection <- newNetworkConnection introductionanswer ownuserid hostname port introductionanswer ownuserid
                        networkconnectionmap <- MVar.takeMVar mvar
                        let newNetworkconnectionmap = Map.insert introductionanswer newConnection networkconnectionmap
                        MVar.putMVar mvar newNetworkconnectionmap
                        used <- MVar.newEmptyMVar
                        MVar.putMVar used False
                        return $ VChan newConnection used

                    _ -> do
                        introductionserial <- NSerialize.serialize introduction
                        Config.traceNetIO $ "Illegal answer from server: " ++ introductionserial
                        threadDelay 1000000
                        initialConnect activeCons mvar hostname port ownport syntype
                Nothing -> do
                    Config.traceNetIO "Something went wrong while connection to the server"
                    threadDelay 1000000
                    initialConnect activeCons mvar hostname port ownport syntype
        Nothing -> do
            Config.traceNetIO "Couldn't connect to server. Retrying"
            threadDelay 1000000
            initialConnect activeCons mvar hostname port ownport syntype

setRedirectRequests :: VChanConnections -> String -> String -> String -> Value -> IO Bool
setRedirectRequests vchanconmvar newhost newport ownport = searchVChans (handleVChan vchanconmvar newhost newport ownport) True (&&)
    where
        handleVChan ::  VChanConnections -> String -> String -> String -> Value -> IO Bool
        handleVChan vchanconmvar newhost newport ownport input = case input of
            VChan nc _ -> do
                Config.traceNetIO $ "Trying to set RedirectRequest for " ++ ncPartnerUserID nc ++ " to " ++ newhost ++ ":" ++ newport

                SSem.withSem (ncHandlingIncomingMessage nc) (do
                    oldconnectionstate <- MVar.takeMVar $ ncConnectionState nc
                    case oldconnectionstate of
                        Connected hostname port partConID ownConID confirmed -> MVar.putMVar (ncConnectionState nc) $ RedirectRequest hostname port newhost newport partConID ownConID confirmed
                        RedirectRequest hostname port _ _ partConID ownConID confirmed -> MVar.putMVar (ncConnectionState nc) $ RedirectRequest hostname port newhost newport partConID ownConID confirmed
                        Emulated partConID ownConID confirmed -> do 
                            Config.traceNetIO "TODO: Allow RedirectRequest for Emulated channel"
                            vchanconnections <- MVar.takeMVar vchanconmvar

                            let userid = ncOwnUserID nc
                            let mbypartner = Map.lookup userid vchanconnections  
                            case mbypartner of
                                Just partner -> do
                                    MVar.putMVar (ncConnectionState nc) $ RedirectRequest "" ownport newhost newport partConID ownConID confirmed -- Setting this to 127.0.0.1 is a temporary hack
                                    oldconectionstatePartner <- MVar.takeMVar $ ncConnectionState partner
                                    MVar.putMVar (ncConnectionState partner) $ Connected newhost newport partConID ownConID confirmed
                                Nothing -> do 
                                    MVar.putMVar (ncConnectionState nc) oldconnectionstate
                                    Config.traceNetIO "Error occured why getting the linked emulated channel"


                            MVar.putMVar vchanconmvar vchanconnections
                        Disconnected partConID ownConID confirmed -> Config.traceNetIO "Cannot set RedirectRequest for a disconnected channel"
                    )
                Config.traceNetIO $ "Set RedirectRequest for " ++ ncPartnerUserID nc ++ " to " ++ newhost ++ ":" ++ newport
                return True
            _ -> return True

serializeVChan :: Value -> IO Value
serializeVChan = modifyVChans handleVChan
    where
        handleVChan :: Value -> IO Value
        handleVChan input = case input of
            VChan nc _-> do
                (r, ro, rl, w, wo, wl, pid, oid, h, p, partConID) <- serializeNetworkConnection nc
                return $ VChanSerial (r, ro, rl) (w, wo, wl) pid oid (h, p, partConID)
            _ -> return input

{-
sendDisconnect :: NMC.ActiveConnections -> MVar.MVar (Map.Map String (NetworkConnection Value)) -> IO ()
sendDisconnect ac mvar = do
    networkConnectionMap <- MVar.readMVar mvar
    let allNetworkConnections = Map.elems networkConnectionMap
    goodbyes <- doForall ac allNetworkConnections
    unless goodbyes $ do
        threadDelay 100000
        sendDisconnect ac mvar
    where
        doForall ac (x:xs) = do
            xres <- sendDisconnectNetworkConnection ac x
            rest <- doForall ac xs
            return $ xres && rest
        doForall ac [] = return True
        sendDisconnectNetworkConnection :: NMC.ActiveConnections -> NetworkConnection Value -> IO Bool
        sendDisconnectNetworkConnection ac con = do
            let writeVals = ncWrite con
            connectionState <- MVar.readMVar $ ncConnectionState con
            -- unreadVals <- DC.unreadMessageStart writeVals
            -- lengthVals <- DC.countMessages writeVals
            -- Config.traceNetIO "Checking if everything is acknowledged"
            -- NB.serialize writeVals >>= Config.traceNetIO . show
            -- NB.isAllAcknowledged writeVals >>= Config.traceNetIO . show
            case connectionState of
                -- Connected host port _ _ _ -> if unreadVals >= lengthVals then do
                Connected host port _ _ _ -> do
                    count <- NB.getNextOffset (ncRead con)
                    if count == 0 then return True else catch (sendNetworkMessage ac con (Messages.AcknowledgeValue (ncOwnUserID con) $ count-1) 0) $ printConErr host port
                    -- ret <- NB.isAllAcknowledged writeVals
                    -- writeValsSer <- NB.serialize writeVals
                    -- Config.traceNetIO $ show writeValsSer ++ "\n    " ++ if ret then "All acknowledged" else "Not completely acknowledged"
                    -- return ret
                    NB.isAllAcknowledged writeVals
                _ -> return True
-}

{-
sendDisconnect :: NMC.ActiveConnections -> MVar.MVar (Map.Map String (NetworkConnection Value)) -> IO ()
sendDisconnect ac mvar = do
    networkConnectionMap <- MVar.readMVar mvar
    let allNetworkConnections = Map.elems networkConnectionMap
    goodbyes <- doForall ac allNetworkConnections
    unless goodbyes $ do
        threadDelay 100000
        sendDisconnect ac mvar
    where
        doForall ac (x:xs) = do
            xres <- sendDisconnectNetworkConnection ac x
            rest <- doForall ac xs
            return $ xres && rest
        doForall ac [] = return True
        sendDisconnectNetworkConnection :: NMC.ActiveConnections -> NetworkConnection Value -> IO Bool
        sendDisconnectNetworkConnection ac con = do
            let writeVals = ncWrite con
            connectionState <- MVar.readMVar $ ncConnectionState con
            -- unreadVals <- DC.unreadMessageStart writeVals
            -- lengthVals <- DC.countMessages writeVals
            -- Config.traceNetIO "Checking if everything is acknowledged"
            -- NB.serialize writeVals >>= Config.traceNetIO . show
            -- NB.isAllAcknowledged writeVals >>= Config.traceNetIO . show
            allAcknowledged <- NB.isAllAcknowledged writeVals
            case connectionState of
                -- Connected host port _ _ _ -> if unreadVals >= lengthVals then do
                Connected host port _ _ _ -> if allAcknowledged then do

                    catch (sendNetworkMessage ac con (Messages.Disconnect $ ncOwnUserID con) 0) $ printConErr host port
                    return True else return False
                _ -> return True
-}

sendDisconnect :: NMC.ActiveConnections -> MVar.MVar (Map.Map String (NetworkConnection Value)) -> IO ()
sendDisconnect ac mvar = do
    networkConnectionMap <- MVar.readMVar mvar
    let allNetworkConnections = Map.elems networkConnectionMap
    goodbyes <- doForall ac allNetworkConnections
    unless goodbyes $ do
        threadDelay 100000
        sendDisconnect ac mvar
    where
        doForall ac (x:xs) = do
            xres <- sendDisconnectNetworkConnection ac x
            rest <- doForall ac xs
            return $ xres && rest
        doForall ac [] = return True
        sendDisconnectNetworkConnection :: NMC.ActiveConnections -> NetworkConnection Value -> IO Bool
        sendDisconnectNetworkConnection ac con = do
            let writeVals = ncWrite con
            connectionState <- MVar.readMVar $ ncConnectionState con
            -- unreadVals <- DC.unreadMessageStart writeVals
            -- lengthVals <- DC.countMessages writeVals
            -- Config.traceNetIO "Checking if everything is acknowledged"
            -- NB.serialize writeVals >>= Config.traceNetIO . show
            -- NB.isAllAcknowledged writeVals >>= Config.traceNetIO . show
            case connectionState of
                Connected {} -> do
                    ret <- NB.isAllAcknowledged writeVals
                    unless ret $ do 
                        serial <- NB.serialize writeVals
                        Config.traceNetIO $ show serial
                    return ret
                _ -> return True
                    