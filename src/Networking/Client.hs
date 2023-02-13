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
import Networking.NetworkConnection
import Control.Concurrent
import Control.Exception
import GHC.Exception
import qualified Syntax
import qualified Networking.Common as NC
import qualified Networking.NetworkConnection as NCon
import qualified Control.Concurrent as MVar
import qualified Config
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

sendValue :: VChanConnections -> NMC.ActiveConnections -> NetworkConnection Value -> Value -> String -> Int -> IO ()
sendValue vchanconsmvar activeCons networkconnection val ownport resendOnError = do
    connectionstate <- MVar.readMVar $ ncConnectionState networkconnection
    case connectionstate of
        NCon.Connected hostname port -> do
            setRedirectRequests vchanconsmvar hostname port ownport val
            valcleaned <- replaceVChan val
            DC.writeMessage (ncWrite networkconnection) valcleaned
            messagesCount <- DC.countMessages $ ncWrite networkconnection
            tryToSendNetworkMessage activeCons networkconnection hostname port (Messages.NewValue (Data.Maybe.fromMaybe "" $ ncOwnUserID networkconnection) messagesCount valcleaned) resendOnError
            disableVChans val
        NCon.Emulated -> do
            vchancons <- MVar.readMVar vchanconsmvar
            valCleaned <- replaceVChan val
            DC.writeMessage (ncWrite networkconnection) valCleaned
            let partnerid = Data.Maybe.fromMaybe "" $ ncPartnerUserID networkconnection
            let mbypartner = Map.lookup partnerid vchancons
            case mbypartner of
                Just partner -> DC.writeMessage (ncRead partner) valCleaned
                _ -> Config.traceNetIO "Something went wrong when sending over a emulated connection"
            disableVChans val
        _ -> Config.traceNetIO "Error when sending message: This channel is disconnected"

sendNetworkMessage :: NMC.ActiveConnections -> NetworkConnection Value -> Messages -> Int -> IO ()
sendNetworkMessage activeCons networkconnection message resendOnError = do
    connectionstate <- MVar.readMVar $ ncConnectionState networkconnection
    case connectionstate of
        NCon.Connected hostname port -> do
            tryToSendNetworkMessage activeCons networkconnection hostname port message resendOnError
        _ -> Config.traceNetIO "Error when sending message: This channel is disconnected"

tryToSendNetworkMessage :: NMC.ActiveConnections -> NetworkConnection Value -> String -> String -> Messages -> Int -> IO ()
tryToSendNetworkMessage activeCons networkconnection hostname port message resendOnError = do
    serializedMessage <- NSerialize.serialize message
    --Config.traceNetIO $ "Sending message as: " ++ Data.Maybe.fromMaybe "" (ncOwnUserID networkconnection) ++ " to: " ++  Data.Maybe.fromMaybe "" (ncPartnerUserID networkconnection)
    --Config.traceNetIO $ "    Over: " ++ hostname ++ ":" ++ port
    --Config.traceNetIO $ "    Message: " ++ serializedMessage
    sendingNetLog serializedMessage $ "Sending message as: " ++ Data.Maybe.fromMaybe "" (ncOwnUserID networkconnection) ++ " to: " ++  Data.Maybe.fromMaybe "" (ncPartnerUserID networkconnection) ++ " Over: " ++ hostname ++ ":" ++ port

    mbycon <- NC.startConversation activeCons hostname port 10000 10
    mbyresponse <- case mbycon of
        Just con -> do
            sendingNetLog serializedMessage "Aquired connection"
            NC.sendMessage con message
            sendingNetLog serializedMessage "Sent message"
            potentialResponse <- NC.recieveResponse con 10000 1000
            sendingNetLog serializedMessage "Recieved response"
            NC.endConversation con 10000 10
            sendingNetLog serializedMessage "Ended connection"
            return potentialResponse
        Nothing -> do 
            sendingNetLog serializedMessage "Connecting unsuccessful"
            return Nothing

    case mbyresponse of
        Just response -> case response of
            Okay -> sendingNetLog serializedMessage "Message okay"    -- Config.traceNetIO $ "Message okay: "++serializedMessage
            OkaySync historyraw -> do
                -- let history = map (setPartnerHostAddress  historyraw
                -- let history = historyraw
                let history = map (setPartnerHostAddress $ NC.getPartnerHostaddress $ Data.Maybe.fromJust mbycon) historyraw
                --Config.traceNetIO $ "Message okay: "++serializedMessage
                serializedResponse <- NSerialize.serialize response
                -- Config.traceNetIO $ "Got syncronization values: "++serializedResponse
                DC.syncMessages (ncRead networkconnection) history
                sendingNetLog serializedMessage $ "Message okay; Got syncronization values: "++serializedResponse
            Redirect host port -> do
                sendingNetLog serializedMessage "Communication partner changed address, resending"
                NCon.changePartnerAddress networkconnection host port
                tryToSendNetworkMessage activeCons networkconnection host port message resendOnError
            Wait -> do
                sendingNetLog serializedMessage "Communication out of sync lets wait!"
                threadDelay 1000000
                tryToSendNetworkMessage activeCons networkconnection hostname port message resendOnError
            _ -> sendingNetLog serializedMessage "Unknown communication error"

        Nothing -> do
            sendingNetLog serializedMessage "Error when recieving response"
            connectionstate <- MVar.readMVar $ ncConnectionState networkconnection
            when (Data.Maybe.isNothing mbycon) $ sendingNetLog serializedMessage "Not connected to peer"
            -- Config.traceNetIO $ "Original message: " ++ serializedMessage
            case connectionstate of
                NCon.Connected newhostname newport -> do
                    {-
                    isClosed <- case mbycon of
                        Just con -> NC.isClosed con
                        Nothing -> return True
                    if resendOnError /= 0 && not isClosed then do
                        sendingNetLog serializedMessage $ "Connected but no answer recieved! New communication partner: " ++ newhostname ++ ":" ++ newport
                        threadDelay 50000
                        tryToSendNetworkMessage activeCons networkconnection newhostname newport message $ max (resendOnError-1) (-1)
                        else sendingNetLog serializedMessage "Old communication partner offline! No longer retrying"
                    -}
                    sendingNetLog serializedMessage $ "Connected but no answer recieved! New communication partner: " ++ newhostname ++ ":" ++ newport
                    threadDelay 500000
                    tryToSendNetworkMessage activeCons networkconnection newhostname newport message $ max (resendOnError-1) (-1)

                _ -> sendingNetLog serializedMessage "Error when sending message: This channel is disconnected while sending"
    sendingNetLog serializedMessage "Message got send or finally failed!"


sendingNetLog :: String -> String -> IO ()
sendingNetLog msg info = Config.traceNetIO $ "Sending message: "++msg++" \n    Status: "++info

setPartnerHostAddress ::  String -> Value -> Value
setPartnerHostAddress address input = case input of
    VSend v -> VSend $ setPartnerHostAddress address v
    VPair v1 v2 ->
        let nv1 = setPartnerHostAddress address v1 in
        let nv2 = setPartnerHostAddress address v2 in
        VPair nv1 nv2
    VFunc penv a b -> 
        let newpenv = setPartnerHostAddressPEnv address penv in
        VFunc newpenv a b
    VDynCast v g -> VDynCast (setPartnerHostAddress address v) g
    VFuncCast v a b -> VFuncCast (setPartnerHostAddress address v) a b
    VRec penv a b c d -> 
        let newpenv = setPartnerHostAddressPEnv address penv in
        VRec newpenv a b c d 
    VNewNatRec penv a b c d e f g -> 
        let newpenv = setPartnerHostAddressPEnv address penv in
        VNewNatRec newpenv a b c d e f g
    VChanSerial r w p o c -> do
        let (hostname, port) = c
        VChanSerial r w p o (if hostname == "" then address else hostname, port)
    _ -> input -- return input
    where
        setPartnerHostAddressPEnv :: String -> [(String, Value)] -> [(String, Value)]
        setPartnerHostAddressPEnv _ [] = []
        setPartnerHostAddressPEnv clientHostaddress penvs@(x:xs) =
            let newval = setPartnerHostAddress clientHostaddress $ snd x in
            (fst x, newval):setPartnerHostAddressPEnv clientHostaddress xs


printConErr :: String -> String -> IOException -> IO ()
printConErr hostname port err = Config.traceIO $ "Communication Partner " ++ hostname ++ ":" ++ port ++ "not found!"


initialConnect :: NMC.ActiveConnections -> MVar.MVar (Map.Map String (NetworkConnection Value)) -> String -> String -> String -> (Syntax.Type, Syntax.Type) -> IO Value
initialConnect activeCons mvar hostname port ownport syntype= do
    mbycon <- NC.waitForConversation activeCons hostname port 1000 100  -- This should be 10000 100 in the real world, expecting just a 100ms ping in the real world might be a little aggressive.

    case mbycon of
        Just con -> do
            ownuserid <- UserID.newRandomUserID
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
                        newConnection <- newNetworkConnection introductionanswer ownuserid hostname port
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


setRedirectRequests :: VChanConnections -> String -> String -> String -> Value -> IO ()
setRedirectRequests vchanconmvar newhost newport ownport input = case input of
    VSend v -> setRedirectRequests vchanconmvar newhost newport ownport v
    VPair v1 v2 -> do
        setRedirectRequests vchanconmvar newhost newport ownport v1
        setRedirectRequests vchanconmvar newhost newport ownport v2
    VFunc penv a b -> setRedirectRequestsPEnv vchanconmvar newhost newport ownport penv
    VDynCast v g -> setRedirectRequests vchanconmvar newhost newport ownport v
    VFuncCast v a b -> setRedirectRequests vchanconmvar newhost newport ownport v
    VRec penv a b c d -> setRedirectRequestsPEnv vchanconmvar newhost newport ownport penv
    VNewNatRec penv a b c d e f g -> setRedirectRequestsPEnv vchanconmvar newhost newport ownport penv
    VChan nc _ -> do
        Config.traceNetIO $ "Trying to set RedirectRequest for " ++ (Data.Maybe.fromMaybe "" $ ncPartnerUserID nc) ++ " to " ++ newhost ++ ":" ++ newport

        SSem.withSem (ncHandlingIncomingMessage nc) (do
            oldconnectionstate <- MVar.takeMVar $ ncConnectionState nc
            case oldconnectionstate of
                Connected hostname port -> MVar.putMVar (ncConnectionState nc) $ NCon.RedirectRequest hostname port newhost newport
                RedirectRequest hostname port _ _ -> MVar.putMVar (ncConnectionState nc) $ NCon.RedirectRequest hostname port newhost newport
                Emulated -> do 
                    Config.traceNetIO "TODO: Allow RedirectRequest for Emulated channel"
                    vchanconnections <- MVar.takeMVar vchanconmvar

                    let userid = ncOwnUserID nc
                    let mbypartner = Map.lookup (Data.Maybe.fromMaybe "" userid) vchanconnections  
                    case mbypartner of
                        Just partner -> do
                            MVar.putMVar (ncConnectionState nc) $ NCon.RedirectRequest "" ownport newhost newport -- Setting this to 127.0.0.1 is a temporary hack
                            oldconectionstatePartner <- MVar.takeMVar $ ncConnectionState partner
                            MVar.putMVar (ncConnectionState partner) $ NCon.Connected newhost newport
                        Nothing -> do 
                            MVar.putMVar (ncConnectionState nc) oldconnectionstate
                            Config.traceNetIO "Error occured why getting the linked emulated channel"


                    MVar.putMVar vchanconmvar vchanconnections
                Disconnected -> Config.traceNetIO "Cannot set RedirectRequest for a disconnected channel"
            )
        Config.traceNetIO $ "Set RedirectRequest for " ++ (Data.Maybe.fromMaybe "" $ ncPartnerUserID nc) ++ " to " ++ newhost ++ ":" ++ newport
    _ -> return ()
    where
        setRedirectRequestsPEnv :: VChanConnections -> String -> String -> String -> [(String, Value)] -> IO ()
        setRedirectRequestsPEnv _ _ _ _ [] = return ()
        setRedirectRequestsPEnv vchanconmvar newhost newport ownport (x:xs) = do
            setRedirectRequests vchanconmvar newhost newport ownport $ snd x
            setRedirectRequestsPEnv vchanconmvar newhost newport ownport xs

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
    VChan nc _-> do
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