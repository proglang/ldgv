module Networking.NetworkingMethod.Fast where

import Networking.NetworkingMethod.NetworkingMethodCommon
import Network.Socket
import GHC.IO.Handle
import System.IO
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.Chan as Chan
import qualified Data.Maybe
import qualified Data.Map as Map
import Control.Concurrent
import Control.Monad

import Networking.Messages
import Networking.NetworkConnection
import Networking.UserID
import qualified Syntax
import qualified Networking.Serialize as NSerialize
import qualified ValueParsing.ValueTokens as VT
import qualified ValueParsing.ValueGrammar as VG
import qualified Config
import qualified Networking.NetworkingMethod.Stateless as Stateless
import ProcessEnvironmentTypes

type Conversation = (String, Handle, MVar.MVar (Map.Map String (String, Responses)))

-- type NetworkAddress = (String, String)
--  deriving (Eq, Show, Ord)

-- type Connectionhandler = MVar.MVar (Map.Map String (NetworkConnection Value)) -> MVar.MVar [(String, Syntax.Type)] -> (Socket, SockAddr) -> Handle -> String -> String -> Messages -> IO ()


sendMessage ::  Conversation -> Messages -> IO ()
sendMessage conversation@(cid, handle, responses) value = Stateless.sendMessage handle (ConversationMessage cid value) 

sendResponse :: Conversation -> Responses -> IO ()
sendResponse conversation@(cid, handle, responses) value = Stateless.sendResponse handle (ConversationResponse cid value) 

conversationHandler :: Handle -> IO Connection
conversationHandler handle = do
    chan <- Chan.newChan 
    mvar <- MVar.newEmptyMVar
    forkIO $ forever $ Stateless.recieveMessageInternal handle VG.parseConversation (\_ -> return ()) (\mes des -> case des of
        ConversationMessage cid message -> Chan.writeChan chan (cid, (mes, message))
        ConversationResponse cid response -> do
            mymap <- MVar.takeMVar mvar
            MVar.putMVar mvar $ Map.insert cid (mes, response) mymap
        )
    return (handle, chan, mvar)


recieveResponse :: Conversation -> Int -> Int -> IO (Maybe Responses)
recieveResponse conversation@(cid, handle, mvar) waitTime tries = do
    responsesMap <- MVar.takeMVar mvar
    case Map.lookup cid responsesMap of
        Just (messages, deserial) -> do 
            MVar.putMVar mvar $ Map.delete cid responsesMap
            return $ Just deserial
        Nothing -> do 
            MVar.putMVar mvar responsesMap
            if tries /= 0 then recieveResponse conversation waitTime $ max (tries-1) (-1) else return Nothing

recieveNewMessage :: Connection -> IO (Conversation, String, Messages)
recieveNewMessage connection@(handle, chan, mvar) = do
    (cid, (serial, deserial)) <- Chan.readChan chan
    return ((cid, handle, mvar), serial, deserial)
    

startConversation :: ActiveConnectionsFast -> String -> String -> Int -> Int -> IO (Maybe Conversation)
startConversation acmvar hostname port waitTime tries = do
    conversationid <- newRandomUserID
    connectionMap <- MVar.takeMVar acmvar
    case Map.lookup (hostname, port) connectionMap of
        Just (handle, chan, mvar) -> do
            MVar.putMVar acmvar connectionMap
            return $ Just (conversationid, handle, mvar)
        Nothing -> do
            statelessActiveCons <- Stateless.createActiveConnections 
            mbyNewHandle <- Stateless.startConversation statelessActiveCons hostname port waitTime tries
            case mbyNewHandle of
                Just handle -> do 
                    newconnection@(handle, chan, mvar) <- conversationHandler handle
                    MVar.putMVar acmvar $ Map.insert (hostname, port) newconnection connectionMap
                    return $ Just (conversationid, handle, mvar)
                Nothing -> do 
                    MVar.putMVar acmvar connectionMap
                    return Nothing

createActiveConnections :: IO ActiveConnectionsFast
createActiveConnections = do
    activeConnections <- MVar.newEmptyMVar
    MVar.putMVar activeConnections Map.empty
    return activeConnections

{-
acceptConversations :: ActiveConnectionsFast -> ConnectionHandler -> Int -> IO (MVar.MVar (Map.Map String (NetworkConnection Value)), MVar.MVar [(String, Syntax.Type)])
acceptConversations ActiveConnectionsFast connectionhandler port = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    let hints = defaultHints {
            addrFamily = AF_INET
          , addrFlags = [AI_PASSIVE]
          , addrSocketType = Stream
    }
    addrInfo <- getAddrInfo (Just hints) Nothing $ Just $ show port
    bind sock $ addrAddress $ head addrInfo
    listen sock 1024
    mvar <- MVar.newEmptyMVar
    MVar.putMVar mvar Map.empty
    clientlist <- MVar.newEmptyMVar
    MVar.putMVar clientlist []
    forkIO $ acceptClients connectionhandler mvar clientlist sock $ show port
    return (mvar, clientlist)
    where
        acceptClients :: ActiveConnectionsFast -> ConnectionHandler -> MVar.MVar (Map.Map String (NetworkConnection Value)) -> MVar.MVar [(String, Syntax.Type)] -> Socket -> String -> IO ()
        acceptClients ActiveConnectionsFast connectionhandler mvar clientlist socket ownport = do
            Config.traceIO "Waiting for clients"
            clientsocket <- accept socket
            Config.traceIO "Accepted new client"

            forkIO $ acceptClient activeConections connectionhandler mvar clientlist clientsocket ownport
            acceptClients ActiveConnectionsFast connectionhandler mvar clientlist socket ownport

        acceptClient :: ActiveConnectionsFast -> ConnectionHandler -> MVar.MVar (Map.Map String (NetworkConnection Value)) -> MVar.MVar [(String, Syntax.Type)] -> (Socket, SockAddr) -> String -> IO ()
        acceptClient ActiveConnectionsFast connectionhandler mvar clientlist clientsocket ownport = do
            hdl <- NC.getHandle $ fst clientsocket
            NC.recieveMessage hdl VG.parseMessages (\_ -> return ()) $ connectionhandler mvar clientlist clientsocket hdl ownport
            hClose hdl  
-}

endConversation :: Conversation -> Int -> Int -> IO ()
endConversation _ _ _ = return ()

