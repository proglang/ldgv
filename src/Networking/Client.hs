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


sendValue :: NMC.ActiveConnections -> NetworkConnection Value -> Value -> Int -> IO ()
sendValue activeCons networkconnection val resendOnError = do
    connectionstate <- MVar.readMVar $ ncConnectionState networkconnection
    case connectionstate of
        NCon.Connected hostname port -> do
            setRedirectRequests hostname port val
            valcleaned <- replaceVChan val
            DC.writeMessage (ncWrite networkconnection) valcleaned
            messagesCount <- DC.countMessages $ ncWrite networkconnection
            catch (do
                tryToSendNetworkMessage activeCons networkconnection hostname port (Messages.NewValue (Data.Maybe.fromMaybe "" $ ncOwnUserID networkconnection) messagesCount valcleaned) resendOnError
                disableVChans val
                ) $ printConErr hostname port
        NCon.Emulated -> DC.writeMessage (ncWrite networkconnection) val
        _ -> Config.traceNetIO "Error when sending message: This channel is disconnected"

sendNetworkMessage :: NMC.ActiveConnections -> NetworkConnection Value -> Messages -> Int -> IO ()
sendNetworkMessage activeCons networkconnection message resendOnError = do
    connectionstate <- MVar.readMVar $ ncConnectionState networkconnection
    case connectionstate of
        NCon.Connected hostname port -> do
            catch ( tryToSendNetworkMessage activeCons networkconnection hostname port message resendOnError) $ printConErr hostname port
        NCon.Emulated -> pure ()
        _ -> Config.traceNetIO "Error when sending message: This channel is disconnected"

tryToSendNetworkMessage :: NMC.ActiveConnections -> NetworkConnection Value -> String -> String -> Messages -> Int -> IO ()
tryToSendNetworkMessage activeCons networkconnection hostname port message resendOnError = do
    serializedMessage <- NSerialize.serialize message
    Config.traceNetIO $ "Sending message as: " ++ Data.Maybe.fromMaybe "" (ncOwnUserID networkconnection) ++ " to: " ++  Data.Maybe.fromMaybe "" (ncPartnerUserID networkconnection)
    Config.traceNetIO $ "    Over: " ++ hostname ++ ":" ++ port
    Config.traceNetIO $ "    Message: " ++ serializedMessage

    mbycon <- NC.startConversation activeCons hostname port 10000 10
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
                threadDelay 100000
                tryToSendNetworkMessage activeCons networkconnection hostname port message resendOnError
            _ -> Config.traceNetIO "Unknown communication error"

        Nothing -> do
            Config.traceNetIO "Error when recieving response"
            connectionstate <- MVar.readMVar $ ncConnectionState networkconnection
            when (Data.Maybe.isNothing mbycon) $ Config.traceNetIO "Not connected to peer"
            Config.traceNetIO $ "Original message: " ++ serializedMessage
            case connectionstate of
                NCon.Connected newhostname newport -> do 
                    isClosed <- case mbycon of
                        Just con -> NC.isClosed con
                        Nothing -> return True
                    if resendOnError /= 0 && not isClosed then do
                        Config.traceNetIO $ "Connected but no answer recieved! New communication partner: " ++ newhostname ++ ":" ++ newport
                        threadDelay 50000
                        tryToSendNetworkMessage activeCons networkconnection newhostname newport message $ max (resendOnError-1) (-1)
                        else Config.traceNetIO "Old communication partner offline! No longer retrying"

                _ -> Config.traceNetIO "Error when sending message: This channel is disconnected while sending"


printConErr :: String -> String -> IOException -> IO ()
printConErr hostname port err = Config.traceIO $ "Communication Partner " ++ hostname ++ ":" ++ port ++ "not found!"


initialConnect :: NMC.ActiveConnections -> MVar.MVar (Map.Map String (NetworkConnection Value)) -> String -> String -> String -> Syntax.Type -> IO Value
initialConnect activeCons mvar hostname port ownport syntype= do
    mbycon <- NC.waitForConversation activeCons hostname port 1000 100  -- This should be 10000 100 in the real world, expecting just a 100ms ping in the real world might be a little aggressive.

    case mbycon of
        Just con -> do
            ownuserid <- UserID.newRandomUserID
            NC.sendMessage con (Messages.IntroduceClient ownuserid ownport syntype)
            mbyintroductionanswer <- NC.recieveResponse con 10000 (-1)
            NC.endConversation con 10000 10
            case mbyintroductionanswer of
                Just introduction -> case introduction of
                    OkayIntroduce introductionanswer -> do
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


setRedirectRequests :: String -> String -> Value -> IO ()
setRedirectRequests newhost newport input = case input of
    VSend v -> setRedirectRequests newhost newport v
    VPair v1 v2 -> do
        setRedirectRequests newhost newport v1
        setRedirectRequests newhost newport v2
    VFunc penv a b -> setRedirectRequestsPEnv newhost newport penv
    VDynCast v g -> setRedirectRequests newhost newport v
    VFuncCast v a b -> setRedirectRequests newhost newport v
    VRec penv a b c d -> setRedirectRequestsPEnv newhost newport penv
    VNewNatRec penv a b c d e f g -> setRedirectRequestsPEnv newhost newport penv
    VChan nc _ -> do
        Config.traceNetIO $ "Trying to set RedirectRequest for " ++ (Data.Maybe.fromMaybe "" $ ncPartnerUserID nc) ++ " to " ++ newhost ++ ":" ++ newport

        SSem.withSem (ncHandlingIncomingMessage nc) (do 
            oldconnectionstate <- MVar.takeMVar $ ncConnectionState nc
            MVar.putMVar (ncConnectionState nc) $ NCon.RedirectRequest (NCon.csHostname oldconnectionstate) (NCon.csPort oldconnectionstate) newhost newport
            )
        Config.traceNetIO $ "Set RedirectRequest for " ++ (Data.Maybe.fromMaybe "" $ ncPartnerUserID nc) ++ " to " ++ newhost ++ ":" ++ newport
    _ -> return ()
    where
        setRedirectRequestsPEnv :: String -> String -> [(String, Value)] -> IO ()
        setRedirectRequestsPEnv _ _ [] = return ()
        setRedirectRequestsPEnv newhost newport (x:xs) = do
            setRedirectRequests newhost newport $ snd x
            setRedirectRequestsPEnv newhost newport xs

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