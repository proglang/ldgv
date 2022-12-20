{-# LANGUAGE LambdaCase #-}

module Networking.Common where

import Network.Socket
import GHC.IO.Handle
import System.IO
import qualified Networking.Serialize as NSerialize 
import qualified ValueParsing.ValueTokens as VT
import qualified ValueParsing.ValueGrammar as VG
import qualified Config

sendMessage :: NSerialize.Serializable a => a -> Handle -> IO ()
sendMessage value handle = do
    serializedValue <- NSerialize.serialize value
    hPutStrLn handle (serializedValue ++" ")

getHandle :: Socket -> IO Handle
getHandle socket = do
    hdl <- socketToHandle socket ReadWriteMode
    hSetBuffering hdl NoBuffering
    return hdl

-- recieveMessage :: Handle -> IO (Maybe a)
recieveMessage :: Handle -> VT.Alex t -> (String -> IO b) -> (String -> t -> IO b) -> IO b
recieveMessage handle grammar fallbackResponse messageHandler = do
    message <- hGetLine handle
    case VT.runAlex message grammar of
        Left err -> do 
            Config.traceIO $ "Error during recieving a networkmessage: "++err
            fallbackResponse message 
        Right deserialmessage -> messageHandler message deserialmessage

openSocketNC :: AddrInfo -> IO Socket
openSocketNC addr = socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)