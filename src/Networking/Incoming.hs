{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
module Networking.Incoming where

import Control.Concurrent
import Control.Monad
import Network.Socket
import Networking.Messages
import Networking.NetworkConnection
import ProcessEnvironmentTypes
import qualified Config
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.SSem as SSem
import qualified Data.Bifunctor
import qualified Data.Map as Map
import qualified Data.Maybe
import qualified Networking.Common as NC
import qualified Networking.Messages as Messages
import qualified Networking.NetworkBuffer as NB
import qualified Networking.NetworkConnection as NCon
import qualified Networking.NetworkingMethod.NetworkingMethodCommon as NMC
import qualified Networking.Outgoing as NO
import qualified Networking.RandomID as RandomID
import qualified Networking.Serialize as NSerialize
import qualified Syntax

handleClient :: NMC.ActiveConnections -> MVar.MVar (Map.Map String (NetworkConnection Value)) -> MVar.MVar [(String, (Syntax.Type, Syntax.Type))] -> (Socket, SockAddr) -> NC.ConversationOrHandle -> String -> String -> Message -> IO ()
handleClient activeCons mvar clientlist clientsocket hdl ownport message deserialmessages = do
    let userid = getUserID deserialmessages
    clientHostaddress <- case snd clientsocket of
        SockAddrInet _ hostname -> return $ hostaddressTypeToString hostname
        _ -> do
            recievedNetLog message "Error during recieving a networkmessage: only ipv4 is currently supported!"
            return ""

    netcons <- MVar.readMVar mvar
    case Map.lookup userid netcons of
        Just ncToPartner -> do
            recievedNetLog message $ "Recieved message as: " ++ ncOwnUserID ncToPartner ++ " (" ++ ownport ++ ") from: " ++  ncPartnerUserID ncToPartner
            busy <- SSem.tryWait $ ncHandlingIncomingMessage ncToPartner
            case busy of
                Just num -> do
                    constate <- MVar.readMVar $ ncConnectionState ncToPartner
                    reply <- case constate of
                        RedirectRequest _ _ host port _ _ _ -> do
                            recievedNetLog message $ "Found redirect request for: " ++ userid
                            recievedNetLog message $ "Send redirect to:" ++ host ++ ":" ++ port
                            SSem.signal $ ncHandlingIncomingMessage ncToPartner
                            NC.sendResponse hdl (Messages.Redirect host port)
                        Connected {} -> case deserialmessages of
                            NewValue userid count val -> do
                                -- DC.lockInterpreterReads (ncRead ncToPartner)
                                success <- NB.writeIfNext (ncRead ncToPartner) count $ setPartnerHostAddress clientHostaddress val
                                SSem.signal $ ncHandlingIncomingMessage ncToPartner
                                if success then recievedNetLog message "Message written to Channel" else recievedNetLog message "Message not correct"
                                NC.sendResponse hdl Messages.Okay
                                recievedNetLog message "Sent okay"
                                -- DC.unlockInterpreterReads (ncRead ncToPartner)
                            RequestValue userid count -> do
                                SSem.signal $ ncHandlingIncomingMessage ncToPartner
                                NC.sendResponse hdl Messages.Okay
                                mbyval <- NB.tryGetAtNB (NCon.ncWrite ncToPartner) count
                                Data.Maybe.maybe (return False) (\val -> NO.sendNetworkMessage activeCons ncToPartner (Messages.NewValue (ncOwnUserID ncToPartner) count val) 0) mbyval
                                return ()
                            AcknowledgeValue userid count -> do
                                NC.sendResponse hdl Messages.Okay -- This okay is needed here to fix a race-condition with disconnects being faster than the okay
                                -- NB.serialize (ncWrite ncToPartner) >>= \x -> Config.traceNetIO $ "Online before acknowlegment: " ++ show x
                                NB.updateAcknowledgements (NCon.ncWrite ncToPartner) count
                                SSem.signal $ ncHandlingIncomingMessage ncToPartner
                            NewPartnerAddress userid port connectionID -> do
                                recievedNetLog message $ "Trying to change the address to: " ++ clientHostaddress ++ ":" ++ port
                                NCon.changePartnerAddress ncToPartner clientHostaddress port connectionID
                                SSem.signal $ ncHandlingIncomingMessage ncToPartner
                                NC.sendResponse hdl Messages.Okay

                                NO.sendNetworkMessage activeCons ncToPartner (Messages.AcknowledgePartnerAddress (ncOwnUserID ncToPartner) connectionID) 0
                                return ()
                            AcknowledgePartnerAddress userid connectionID -> do
                                conConfirmed <- NCon.confirmConnectionID ncToPartner connectionID
                                SSem.signal $ ncHandlingIncomingMessage ncToPartner
                                if conConfirmed then NC.sendResponse hdl Messages.Okay else NC.sendResponse hdl Messages.Error
                            Disconnect userid -> do
                                NC.sendResponse hdl Messages.Okay
                                NCon.disconnectFromPartner ncToPartner
                                SSem.signal $ ncHandlingIncomingMessage ncToPartner
                                return ()
                            _ -> do
                                serial <- NSerialize.serialize deserialmessages
                                recievedNetLog message $ "Error unsupported networkmessage: "++ serial
                                SSem.signal $ ncHandlingIncomingMessage ncToPartner
                                NC.sendResponse hdl Messages.Okay
                        _ -> do
                            recievedNetLog message "Network Connection is in a illegal state!"
                            SSem.signal $ ncHandlingIncomingMessage ncToPartner
                            NC.sendResponse hdl Messages.Okay
                    return reply
                Nothing -> do
                    recievedNetLog message "Message cannot be handled at the moment! Sending wait response"
                    SSem.signal $ ncHandlingIncomingMessage ncToPartner
                    NC.sendResponse hdl Messages.Wait

        Nothing -> do
            recievedNetLog message "Recieved message from unknown connection"
            case deserialmessages of
                Introduce userid clientport synname syntype -> do
                    serverid <- RandomID.newRandomID
                    newpeer <- newNetworkConnection userid serverid clientHostaddress clientport userid serverid
                    NC.sendResponse hdl (Messages.OkayIntroduce serverid)
                    repserial <- NSerialize.serialize $ Messages.OkayIntroduce serverid
                    recievedNetLog message $ "    Response to "++ userid ++ ": " ++ repserial

                    recievedNetLog message "Patching MVar"
                    netcons <- MVar.takeMVar mvar
                    MVar.putMVar mvar $ Map.insert userid newpeer netcons


                    clientlistraw <- MVar.takeMVar clientlist
                    MVar.putMVar clientlist $ clientlistraw ++ [(userid, (synname, syntype))]
                    -- We must not write clients into the clientlist before adding them to the networkconnectionmap
                
                _ -> do
                    serial <- NSerialize.serialize deserialmessages
                    recievedNetLog message $ "Error unsupported networkmessage: "++ serial
                    recievedNetLog message "This is probably a timing issue! Lets resend later"
                    NC.sendResponse hdl Messages.Wait


    recievedNetLog message "Message successfully handled"

recievedNetLog :: String -> String -> IO ()
recievedNetLog msg info = Config.traceNetIO $ "Recieved message: "++msg++" \n    Status: "++info

setPartnerHostAddress ::  String -> Value -> Value
setPartnerHostAddress address = modifyVChansStatic (handleSerial address)
    where
        handleSerial :: String -> Value -> Value
        handleSerial address input = case input of
            VChanSerial r w p o c -> do
                let (hostname, port, partnerID) = c
                VChanSerial r w p o (if hostname == "" then address else hostname, port, partnerID)
            _ -> input -- return input

waitUntilContactedNewPeers :: VChanConnections -> NMC.ActiveConnections -> NetworkConnection Value -> Value -> String -> IO ()
waitUntilContactedNewPeers vchansmvar activeCons ownNC input ownport = do
    contactedPeers <- contactNewPeers vchansmvar activeCons ownport ownNC input
    unless contactedPeers $ do
        threadDelay 50000
        waitUntilContactedNewPeers vchansmvar activeCons ownNC input ownport

contactNewPeers :: VChanConnections -> NMC.ActiveConnections -> String -> NetworkConnection Value ->  Value -> IO Bool
contactNewPeers vchansmvar activeCons ownport ownNC = searchVChans (handleVChan activeCons ownport ownNC) True (&&)
    where
        handleVChan :: NMC.ActiveConnections -> String -> NetworkConnection Value -> Value -> IO Bool
        handleVChan activeCons ownport ownNC input = case input of
            VChan nc bool -> do
                connectionState <- MVar.readMVar $ ncConnectionState nc
                case connectionState of
                    Emulated {} -> return True
                    _ -> do
                        if csConfirmedConnection connectionState then return True else do
                            -- Check whether their partner is also registered and connected on this instance, if so convert the connection into a emulated one
                            vchanconnections <- MVar.readMVar vchansmvar
                            let userid = ncOwnUserID nc
                            let partnerid = ncPartnerUserID nc
                            let mbypartner = Map.lookup userid vchanconnections  
                            case mbypartner of
                                Just partner -> do
                                    -- Their partner is registered in this instance. Now we have to figure out whether this is till current and we can start emulating the connection
                                    SSem.wait (ncHandlingIncomingMessage partner) 
                                    connectionstate <- MVar.takeMVar $ ncConnectionState partner
                                    case connectionState of
                                        Connected {} -> do
                                            -- Reemulate them
                                            partConID <- RandomID.newRandomID
                                            ownConID <- RandomID.newRandomID
                                            MVar.putMVar (ncConnectionState partner) $ Emulated ownConID partConID True
                                            _ <- MVar.takeMVar $ ncConnectionState nc
                                            MVar.putMVar (ncConnectionState nc) $ Emulated partConID ownConID True
                                            SSem.signal (ncHandlingIncomingMessage partner)
                                            return True
                                        _ -> do
                                            -- Nothing to do here, we no longer own the partner
                                            MVar.putMVar (ncConnectionState partner) connectionState
                                            SSem.signal (ncHandlingIncomingMessage partner)
                                            sendSuccess <- NO.sendNetworkMessage activeCons nc (Messages.NewPartnerAddress (ncOwnUserID nc) ownport $ csOwnConnectionID connectionState) $ -2
                                            if sendSuccess then return False else do 
                                                threadDelay 100000
                                                putStrLn "Trying to lookup future messages"
                                                futureRecieveContainsPartner ownNC partnerid
                                Nothing -> do 
                                    -- Their partner isnt registered in this instance
                                    sendSuccess <- NO.sendNetworkMessage activeCons nc (Messages.NewPartnerAddress (ncOwnUserID nc) ownport $ csOwnConnectionID connectionState) $ -2
                                    if sendSuccess then return False else do
                                        threadDelay 100000
                                        putStrLn "Trying to lookup future messages"
                                        futureRecieveContainsPartner ownNC partnerid
                                    -- return False
            _ -> return True

hostaddressTypeToString :: HostAddress -> String
hostaddressTypeToString hostaddress = do
    let (a, b, c, d) = hostAddressToTuple hostaddress
    show a ++ "." ++ show b ++ "."++ show c ++ "." ++ show d

findFittingClientMaybe :: MVar.MVar [(String, (Syntax.Type, Syntax.Type))] -> (Syntax.Type, Syntax.Type) -> IO (Maybe String)
findFittingClientMaybe clientlist desiredType = do
    clientlistraw <- MVar.takeMVar clientlist
    let newclientlistrawAndReturn = fFCMRaw clientlistraw desiredType
    -- We send the name of the type but not the type itself, this needs to change
    MVar.putMVar clientlist $ fst newclientlistrawAndReturn
    return $ snd newclientlistrawAndReturn
    where
        fFCMRaw :: [(String, (Syntax.Type, Syntax.Type))] -> (Syntax.Type, Syntax.Type) -> ([(String, (Syntax.Type, Syntax.Type))], Maybe String)
        fFCMRaw [] _ = ([], Nothing)
        fFCMRaw (x:xs) desiredtype = if compare (snd x) desiredtype then (xs, Just $ fst x) else do
            let nextfFCMRaw = fFCMRaw xs desiredtype
            Data.Bifunctor.first (x :) nextfFCMRaw

        compare :: (Syntax.Type, Syntax.Type) -> (Syntax.Type, Syntax.Type) -> Bool
        compare a@(aName, aType) b@(bName, bType) = aName == Syntax.dualof bName && aType == bType

-- This halts until a fitting client is found
findFittingClient :: MVar.MVar [(String, (Syntax.Type, Syntax.Type))] -> (Syntax.Type, Syntax.Type) -> IO String
findFittingClient clientlist desiredType = do
    mbystring <- findFittingClientMaybe clientlist desiredType
    case mbystring of
        Just userid -> return userid
        Nothing -> do
            threadDelay 10000 -- Sleep for 10 ms to not hammer the CPU
            findFittingClient clientlist desiredType

replaceVChanSerial :: NMC.ActiveConnections -> MVar.MVar (Map.Map String (NetworkConnection Value)) -> Value -> IO Value
replaceVChanSerial activeCons mvar input = modifyVChans (handleSerial activeCons mvar) input
    where
        handleSerial :: NMC.ActiveConnections -> MVar.MVar (Map.Map String (NetworkConnection Value)) -> Value -> IO Value
        handleSerial activeCons mvar input = case input of
            VChanSerial r w p o c -> do
                networkconnection <- createNetworkConnection r w p o c
                ncmap <- MVar.takeMVar mvar
                MVar.putMVar mvar $ Map.insert p networkconnection ncmap
                used<- MVar.newEmptyMVar
                MVar.putMVar used False
                return $ VChan networkconnection used
            _ -> return input

recieveValue :: VChanConnections -> NMC.ActiveConnections -> NetworkConnection Value -> String -> IO Value
recieveValue vchanconsvar activeCons networkconnection ownport = do
    recieveValueInternal 0 vchanconsvar activeCons networkconnection ownport
    where
        recieveValueInternal :: Int -> VChanConnections -> NMC.ActiveConnections -> NetworkConnection Value -> String -> IO Value
        recieveValueInternal count vchanconsvar activeCons networkconnection ownport = do
            let readDC = ncRead networkconnection
            mbyUnclean <- NB.tryTake readDC
            case mbyUnclean of
                Just unclean -> do
                    val <- replaceVChanSerial activeCons vchanconsvar $ fst unclean
                    waitUntilContactedNewPeers vchanconsvar activeCons networkconnection val ownport
                    -- msgCount <- DC.unreadMessageStart $ ncRead networkconnection
                    connectionState <- MVar.readMVar $ ncConnectionState networkconnection
                    case connectionState of
                        Connected {} -> NO.sendNetworkMessage activeCons networkconnection (Messages.AcknowledgeValue (ncOwnUserID networkconnection) $ snd unclean) $ -1
                        Emulated {} -> do
                            vchancons <- MVar.readMVar vchanconsvar
                            let ownid = ncOwnUserID networkconnection
                            let mbypartner = Map.lookup ownid vchancons
                            case mbypartner of
                                Just partner -> do
                                    -- NB.serialize (ncWrite partner) >>= \x -> Config.traceNetIO $ "Emulated "++ show unclean ++ " before acknowlegment: " ++ show x
                                    NB.updateAcknowledgements (ncWrite partner) $ snd unclean
                                    return True
                                _ -> Config.traceNetIO "Something went wrong when acknowleding value of emulated connection" >> return True
                        _ -> return True

                    return val
                Nothing -> if count == 0 then do
                        msgCount <- NB.getNextOffset $ ncRead networkconnection
                        connectionState <- MVar.readMVar $ ncConnectionState networkconnection
                        case connectionState of
                            Connected {} -> NO.sendNetworkMessage activeCons networkconnection (Messages.RequestValue (ncOwnUserID networkconnection) msgCount) 0
                            _ -> return True
                        recieveValueInternal 100 vchanconsvar activeCons networkconnection ownport
                        else do
                            threadDelay 5000
                            recieveValueInternal (count-1) vchanconsvar activeCons networkconnection ownport

valueContainsPartner :: String -> Value -> IO Bool
valueContainsPartner partner = searchVChans (handleSerial partner) False (||)
    where
        handleSerial :: String -> Value -> IO Bool
        handleSerial partner value = case value of
            VChanSerial r w p o c -> do 
                putStrLn $ "Looking for: " ++ partner ++ " p: " ++ p ++ " o: " ++ o
                return (partner == o)
            _ -> return False

futureRecieveContainsPartner :: NetworkConnection Value -> String -> IO Bool
futureRecieveContainsPartner = fRCPInternal 0 
    where
        fRCPInternal count nc partner = do
            putStrLn $ "Trying to read at: " ++ show count
            mbyVal <- NB.tryGetAtRelativeNB (ncRead nc) count
            case mbyVal of
              Nothing -> do 
                putStrLn $ "Index " ++ show count ++ " is empty"
                return False
              Just value -> do
                putStrLn $ "Looking up index " ++ show count
                containsPartner <- valueContainsPartner partner value
                if containsPartner then return True else fRCPInternal (count+1) nc partner
