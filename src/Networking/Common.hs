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

{-
-- communicate :: Chan.Chan Value -> Chan.Chan Value -> Socket -> IO ()
communicate read write socket = do
    hdl <- socketToHandle socket ReadWriteMode
    hSetBuffering hdl NoBuffering
    forkIO (sendWritten write hdl)
    recieveReadable read hdl
    where
        sendWritten write handle = do
            message <- readChan write
            putStrLn $ "Sending message:" ++ SV.serialize message
            hPutStrLn handle (SV.serialize message ++" ")
            sendWritten write handle

        recieveReadable read handle = do
            message <- hGetLine handle
            putStrLn $ "Recieved message:" ++ message
            case VT.runAlex message VG.parseValues of
                Left err -> putStrLn $ "Error during recieving a networkmessage: "++err
                Right deserial -> writeChan read deserial
            recieveReadable read handle
-}


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
        Just connectioninfo -> return $ ProcessEnvironment.handle connectioninfo
        Nothing -> userIDToHandle mvar userid

sendMessageID :: Value -> MVar.MVar (Map.Map String ConnectionInfo) -> String -> IO ()
sendMessageID value handlemapmvar userid = do
    serializedValue <- NSerialize.serialize $ NewValue userid value
    putStrLn $ "Sending message:" ++ serializedValue
    handle <- userIDToHandle handlemapmvar userid
    hPutStrLn handle  (serializedValue ++ " ")

    {-
    maybehandle <- userIDToHandle handlemapmvar userid
    case maybehandle of
        Just handle -> hPutStrLn handle  (serializedValue ++" ")
        Nothing -> putStrLn $ "Error " ++ userid ++ " not found while trying to recieve messages"
    -}

recieveMessagesID :: Chan.Chan Value -> MVar.MVar (Map.Map String ConnectionInfo) -> String -> IO ()
recieveMessagesID chan mvar userid = do
    handle <- userIDToHandle mvar userid
    message <- hGetLine handle
    putStrLn $ "Recieved message:" ++ message
    case VT.runAlex message VG.parseValues of
        Left err -> putStrLn $ "Error during recieving a networkmessage: "++err
        Right deserial -> writeChan chan deserial
    {-
    case maybehandle of
        Just handle -> do
            message <- hGetLine handle
            putStrLn $ "Recieved message:" ++ message
            case VT.runAlex message VG.parseValues of
                Left err -> putStrLn $ "Error during recieving a networkmessage: "++err
                Right deserial -> writeChan chan deserial
        Nothing -> putStrLn $ "Error " ++ userid ++ " not found while trying to recieve messages"
    -}
    recieveMessagesID chan mvar userid


sendMessage :: NSerialize.Serializable a => a -> Handle -> IO ()
sendMessage value handle = do
    serializedValue <- NSerialize.serialize value
    putStrLn $ "Sending message:" ++ serializedValue
    hPutStrLn handle (serializedValue ++" ")



recieveMessages :: Chan.Chan Value -> Handle -> IO ()
recieveMessages chan handle = do
    message <- hGetLine handle
    putStrLn $ "Recieved message:" ++ message
    case VT.runAlex message VG.parseValues of
        Left err -> putStrLn $ "Error during recieving a networkmessage: "++err
        Right deserial -> writeChan chan deserial
    recieveMessages chan handle


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
