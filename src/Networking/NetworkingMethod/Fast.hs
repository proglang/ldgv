module Networking.NetworkingMethod.Fast where

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
import Networking.UserID
import qualified Networking.Serialize as NSerialize
import qualified ValueParsing.ValueTokens as VT
import qualified ValueParsing.ValueGrammar as VG
import qualified Config
import qualified Networking.NetworkingMethod.Stateless as Stateless
type Conversation = (String, Handle, MVar.MVar (Map.Map String (String, Responses)))

type NetworkAddress = (String, String)
--  deriving (Eq, Show, Ord)

type ActiveConnections = Map.Map NetworkAddress Connection

type Connection = (Handle, Chan.Chan (String, (String, Messages)), MVar.MVar (Map.Map String (String, Responses)))


sendMessage :: Messages -> Conversation -> IO ()
sendMessage value conversation@(cid, handle, responses) = Stateless.sendMessage (ConversationMessage cid value) handle

sendResponse :: Responses  -> Conversation -> IO ()
sendResponse value conversation@(cid, handle, responses) = Stateless.sendResponse (ConversationResponse cid value) handle

conversationHandler :: Handle -> IO Connection
conversationHandler handle = do
    chan <- Chan.newChan 
    mvar <- MVar.newEmptyMVar
    forkIO $ forever $ Stateless.recieveMessage handle VG.parseConversation (\_ -> return ()) (\mes des -> case des of
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
    

startConversation :: MVar.MVar ActiveConnections -> String -> String -> Int -> Int -> IO (Maybe Conversation)
startConversation acmvar hostname port waitTime tries = do
    conversationid <- newRandomUserID
    connectionMap <- MVar.takeMVar acmvar
    case Map.lookup (hostname, port) connectionMap of
        Just (handle, chan, mvar) -> do
            MVar.putMVar acmvar connectionMap
            return $ Just (conversationid, handle, mvar)
        Nothing -> do
            mbyNewHandle <- Stateless.startConversation hostname port waitTime tries
            case mbyNewHandle of
                Just handle -> do 
                    newconnection@(handle, chan, mvar) <- conversationHandler handle
                    MVar.putMVar acmvar $ Map.insert (hostname, port) newconnection connectionMap
                    return $ Just (conversationid, handle, mvar)
                Nothing -> do 
                    MVar.putMVar acmvar connectionMap
                    return Nothing

endConversation :: Conversation -> Int -> Int -> IO ()
endConversation _ _ _ = return ()

