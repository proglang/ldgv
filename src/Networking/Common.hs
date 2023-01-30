{-# LANGUAGE LambdaCase #-}

module Networking.Common where

import Network.Socket
import GHC.IO.Handle
import System.IO
import qualified Networking.Serialize as NSerialize 
import qualified ValueParsing.ValueTokens as VT
import qualified ValueParsing.ValueGrammar as VG
import qualified Config
import qualified Networking.NetworkingMethod.Fast as NetMethod


type ConversationOrHandle = NetMethod.Conversation 

-- type ConversationOrHandle = Handle

-- The compiler sadly compains when these things get eta reduced :/
sendMessage con ser = NetMethod.sendMessage con ser

sendResponse con ser = NetMethod.sendResponse con ser

startConversation activeCons host port waitTime tries = NetMethod.startConversation activeCons host port waitTime tries

waitForConversation activeCons host port waitTime tries = NetMethod.waitForConversation activeCons host port waitTime tries

createActiveConnections = NetMethod.createActiveConnections

acceptConversations activeCons connectionhandler port socketsmvar = NetMethod.acceptConversations activeCons connectionhandler port socketsmvar

recieveResponse con waitTime tries = NetMethod.recieveResponse con waitTime tries

endConversation con waitTime tries = NetMethod.endConversation con waitTime tries

{-
getHandle :: Socket -> IO Handle
getHandle socket = do
    hdl <- socketToHandle socket ReadWriteMode
    hSetBuffering hdl NoBuffering
    return hdl

recieveMessage :: Handle -> VT.Alex t -> (String -> IO b) -> (String -> t -> IO b) -> IO b
recieveMessage handle grammar fallbackResponse messageHandler = do
    message <- hGetLine handle
    case VT.runAlex message grammar of
        Left err -> do 
            Config.traceNetIO $ "Error during recieving a networkmessage: "++err
            fallbackResponse message 
        Right deserialmessage -> do 
            -- Config.traceNetIO $ "New superficially valid message recieved: "++message
            messageHandler message deserialmessage

openSocketNC :: AddrInfo -> IO Socket
openSocketNC addr = socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
-}