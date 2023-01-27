module Networking.NetworkingMethod.Stateless where

import Network.Socket
import GHC.IO.Handle
import System.IO
import qualified Control.Concurrent.MVar as MVar
import qualified Data.Maybe
import Control.Concurrent

import Networking.Messages
import qualified Networking.Serialize as NSerialize
import qualified ValueParsing.ValueTokens as VT
import qualified ValueParsing.ValueGrammar as VG
import qualified Config



sendMessage :: NSerialize.Serializable a => a -> Handle -> IO ()
sendMessage value handle = do
    serializedValue <- NSerialize.serialize value
    hPutStrLn handle (serializedValue ++" ")

sendResponse :: NSerialize.Serializable a => a -> Handle -> IO ()
sendResponse = sendMessage

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

startConversation :: String -> String -> Int -> Int -> IO (Maybe Handle)
startConversation hostname port waitTime tries = do
    let hints = defaultHints {
                addrFamily = AF_INET
              , addrFlags = []
              , addrSocketType = Stream
            }
    handleMVar <- MVar.newEmptyMVar
    threadid <- forkIO (do
        Config.traceNetIO $ "Trying to connect to: " ++ hostname ++":"++port
        addrInfo <- getAddrInfo (Just hints) (Just hostname) $ Just port
        clientsocket <- openSocketNC $ head addrInfo
        connect clientsocket $ addrAddress $ head addrInfo
        handle <- getSocketFromHandle clientsocket
        MVar.putMVar handleMVar handle
        )
    getFromNetworkThread threadid handleMVar waitTime tries


getFromNetworkThread :: ThreadId -> MVar.MVar a -> Int -> Int -> IO (Maybe a)
getFromNetworkThread = getFromNetworkThreadWithModification Just

getFromNetworkThreadWithModification :: (a -> Maybe b) -> ThreadId -> MVar a -> Int -> Int -> IO (Maybe b)
getFromNetworkThreadWithModification func threadid mvar waitTime currentTry = do
        mbyResult <- MVar.tryReadMVar mvar
        case mbyResult of
            Just handle -> return $ func handle
            Nothing -> if currentTry /= 0 then do
                threadDelay waitTime
                getFromNetworkThreadWithModification func threadid mvar waitTime $ max (currentTry-1) (-1)
                else do
                    killThread threadid
                    return Nothing

recieveResponse :: Handle -> Int -> Int -> IO (Maybe Responses)
recieveResponse handle waitTime tries = do
    retVal <- MVar.newEmptyMVar
    threadid <- forkIO $ recieveMessage handle VG.parseResponses (\_ -> MVar.putMVar retVal Nothing) (\_ des -> MVar.putMVar retVal $ Just des)
    getFromNetworkThreadWithModification id threadid retVal waitTime tries

recieveNewMessage :: Handle -> IO (Handle, String, Messages)
recieveNewMessage handle = do
    recieveMessage handle VG.parseMessages (\_ -> recieveNewMessage handle) $ \s des -> return (handle, s, des)
    

endConversation :: Handle -> Int -> Int -> IO ()
endConversation handle waitTime tries = do 
    finished <- MVar.newEmptyMVar
    threadid <- forkIO $ hClose handle >> MVar.putMVar finished True
    _ <- getFromNetworkThread threadid finished waitTime tries
    return ()
    

openSocketNC :: AddrInfo -> IO Socket
openSocketNC addr = socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)

getSocketFromHandle :: Socket -> IO Handle
getSocketFromHandle socket = do
    hdl <- socketToHandle socket ReadWriteMode
    hSetBuffering hdl NoBuffering
    return hdl