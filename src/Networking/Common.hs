{-# LANGUAGE LambdaCase #-}

module Networking.Common where

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Concurrent
import GHC.IO.Handle
import Control.Monad.IO.Class
import System.IO
import qualified Control.Concurrent.Chan as Chan
import qualified Control.Concurrent.MVar as MVar
import ProcessEnvironment

import qualified Networking.Serialize as NSerialize 
import Networking.Messages
import Control.Exception

import qualified ValueParsing.ValueTokens as VT
import qualified ValueParsing.ValueGrammar as VG
import qualified Networking.DirectionalConnection as DC
import Networking.DirectionalConnection (DirectionalConnection)
import Networking.Serialize (Serializable)

newtype ServerException = NoIntroductionException String
    deriving Eq

instance Show ServerException where
    show = \case
        NoIntroductionException s -> "Partner didn't introduce itself, but sent: " ++ s

instance Exception ServerException


-- Hangs if no valid client id is provided
getConnectionInfo :: MVar.MVar (Map String ConnectionInfo) -> String -> IO ConnectionInfo
getConnectionInfo mvar user = do
    dict <- MVar.readMVar mvar
    case Map.lookup user dict of
        Nothing -> getConnectionInfo mvar user
        Just clientinfo -> return clientinfo

-- This waits until the handle is found
userIDToHandle :: MVar.MVar (Map.Map String ConnectionInfo) -> String -> IO Handle
userIDToHandle mvar userid = do
    useridmap <- readMVar mvar
    case Map.lookup userid useridmap of
        Just connectioninfo -> return $ ciHandle connectioninfo
        Nothing -> userIDToHandle mvar userid

sendMessageID :: Value -> MVar.MVar (Map.Map String ConnectionInfo) -> String -> String -> IO ()
sendMessageID value handlemapmvar partnerid userid = do
    serializedValue <- NSerialize.serialize $ NewValue userid value
    putStrLn $ "Sending message:" ++ serializedValue
    handle <- userIDToHandle handlemapmvar partnerid
    hPutStrLn handle  (serializedValue ++ " ")


recieveMessagesID :: DirectionalConnection Value -> MVar.MVar (Map.Map String ConnectionInfo) -> String -> IO ()
recieveMessagesID chan mvar userid = do
    handle <- userIDToHandle mvar userid
    message <- hGetLine handle
    putStrLn $ "Recieved message:" ++ message
    case VT.runAlex message VG.parseMessages of
    -- case VT.runAlex message VG.parseValues of
        Left err -> putStrLn $ "Error during recieving a networkmessage: "++err
        Right deserialmessages -> case deserialmessages of
            NewValue userid val -> do 
                valCleaned <- replaceVChanSerial val -- Replaces VChanSerial with VChans and their appropriate connection
                DC.writeMessage chan valCleaned
            _ -> do
                serial <- NSerialize.serialize deserialmessages
                putStrLn $ "Error unsupported networkmessage: "++ serial
    recieveMessagesID chan mvar userid


sendMessage :: NSerialize.Serializable a => a -> Handle -> IO ()
sendMessage value handle = do
    serializedValue <- NSerialize.serialize value
    putStrLn $ "Sending message:" ++ serializedValue
    hPutStrLn handle (serializedValue ++" ")

recieveMessage :: Handle -> IO (Maybe Messages)
recieveMessage handle = do
    message <- hGetLine handle
    case VT.runAlex message VG.parseMessages of
    -- case VT.runAlex message VG.parseValues of
        Left err -> do 
            putStrLn $ "Error during recieving a networkmessage: "++err
            return Nothing
        Right deserialmessage -> return $ Just deserialmessage



getHandle :: Socket -> IO Handle
getHandle socket = do
    hdl <- socketToHandle socket ReadWriteMode
    hSetBuffering hdl NoBuffering
    return hdl


getSocket :: MVar.MVar Socket -> Socket -> IO ()
getSocket mvar socket = do
    putStrLn "Trying to send socket"
    MVar.putMVar mvar socket
    putStrLn "Sent socket"

waitForServerIntroduction :: Handle -> IO String
waitForServerIntroduction handle = do
    message <- hGetLine handle
    case VT.runAlex message VG.parseMessages of
        Left err -> do 
            putStrLn $ "Error during server introduction: "++err
            throw $ NoIntroductionException message
        Right deserial -> case deserial of
            Introduce partner -> do
                return partner
            _ -> do 
                putStrLn $ "Error during server introduction, wrong message: "++ message
                throw $ NoIntroductionException message


replaceVChanSerial :: Value -> IO Value
replaceVChanSerial input = case input of
    VSend v -> do
        nv <- replaceVChanSerial v
        return $ VSend nv
    VPair v1 v2 -> do 
        nv1 <- replaceVChanSerial v1
        nv2 <- replaceVChanSerial v2
        return $ VPair nv1 nv2
    VFunc penv a b -> do
        newpenv <- replaceVChanSerialPEnv penv
        return $ VFunc newpenv a b
    VDynCast v g -> do 
        nv <- replaceVChanSerial v
        return $ VDynCast nv g
    VFuncCast v a b -> do 
        nv <- replaceVChanSerial v
        return $ VFuncCast nv a b
    VRec penv a b c d -> do 
        newpenv <- replaceVChanSerialPEnv penv
        return $ VRec newpenv a b c d
    VNewNatRec penv a b c d e f g -> do 
        newpenv <- replaceVChanSerialPEnv penv
        return $ VNewNatRec newpenv a b c d e f g
    VChanSerial r ri w wi pid oid p h -> do 
        putStrLn "Attempting to deserialize a VChanSerial"
        getVChanFromSerial r ri w wi pid oid p h
    _ -> return input
    where
        replaceVChanSerialPEnv :: [(String, Value)] -> IO [(String, Value)]
        replaceVChanSerialPEnv [] = return []
        replaceVChanSerialPEnv (x:xs) = do 
            newval <- replaceVChanSerial $ snd x
            rest <- replaceVChanSerialPEnv xs
            return $ (fst x, newval):rest

getVChanFromSerial :: [Value] -> Int -> [Value] -> Int -> String -> String -> String -> String -> IO Value
getVChanFromSerial msgRead readCount msgWrite writeCount partnerID ownID port hostname = do
  readDC <- DC.createConnection msgRead readCount
  writeDC <- DC.createConnection msgWrite writeCount
  channelstate <- MVar.newEmptyMVar 

  -- Connect to partner
  let hints = defaultHints {
                addrFlags = []
              , addrSocketType = Stream
  }

  putStrLn $ "getVChanFromSerial: Trying to connect to new partner: " ++ hostname ++ ":" ++ port
  addrInfo <- getAddrInfo (Just hints) (Just hostname) $ Just port
  clientsocket <- openSocket $ head addrInfo
  putStrLn "getVChanFromSerial: Aquired socket"
  connect clientsocket $ addrAddress $ head addrInfo
  putStrLn "getVChanFromSerial: Connected to socket"
  handle <- getHandle clientsocket
  putStrLn "getVChanFromSerial: Converted to handle"
  sendMessage (Introduce ownID) handle
  putStrLn "getVChanFromSerial: Waiting for handshake"
  serverid <- waitForServerIntroduction handle
  putStrLn "getVChanFromSerial: Handshake recieved"
  if partnerID == serverid then do
    putStrLn "getVChanFromSerial: Handshake valid"
    -- Hookup automatic message recieving
    mvar <- liftIO MVar.newEmptyMVar
    MVar.putMVar mvar (Map.fromList [(serverid, ConnectionInfo handle (addrAddress $ head addrInfo) readDC writeDC )])
    forkIO $ recieveMessagesID readDC mvar serverid
    MVar.putMVar channelstate $ Connected mvar
    putStrLn "getVChanFromSerial: Message revieving hooked up"
  else MVar.putMVar channelstate Disconnected
  return $ VChan $ CommunicationChannel readDC writeDC (Just partnerID) (Just ownID) channelstate


openSocket addr = socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)