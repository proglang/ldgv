module Networking.NetworkingMethod.Fast where

import Control.Concurrent
import Control.Exception
import Control.Monad
import GHC.IO.Handle
import Network.Socket
import Networking.Messages
import Networking.NetworkConnection
import Networking.NetworkingMethod.NetworkingMethodCommon
import Networking.RandomID
import ProcessEnvironmentTypes
import qualified Config
import qualified Control.Concurrent.Chan as Chan
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.SSem as SSem
import qualified Data.Map as Map
import qualified Networking.NetworkingMethod.Stateless as Stateless
import qualified Syntax
import qualified ValueParsing.ValueGrammar as VG

type ResponseMapMVar = MVar.MVar (Map.Map String (String, Response))

data Conversation = Conversation {convID :: String, convHandle :: Stateless.Conversation, convRespMap :: ResponseMapMVar, convSending :: SSem.SSem}

type ConnectionHandler = ActiveConnectionsFast -> MVar.MVar (Map.Map String (NetworkConnection Value)) -> MVar.MVar [(String, (Syntax.Type, Syntax.Type))] -> (Socket, SockAddr) -> Conversation -> String -> String -> Message -> IO ()

sendMessage ::  Conversation -> Message -> IO ()
sendMessage conv value = SSem.withSem (convSending conv) $ Stateless.sendMessage (convHandle conv) (ConversationMessage (convID conv) value)

sendResponse :: Conversation -> Response -> IO ()
sendResponse conv value = SSem.withSem (convSending conv) $ Stateless.sendResponse (convHandle conv) (ConversationResponse (convID conv) value)

conversationHandler :: Stateless.Conversation -> IO Connection
conversationHandler handle = do
    chan <- Chan.newChan
    mvar <- MVar.newEmptyMVar
    MVar.putMVar mvar Map.empty
    sem <- SSem.new 1
    conversationHandlerChangeHandle handle chan mvar sem

conversationHandlerChangeHandle :: Stateless.Conversation
                                     -> Chan (ConversationID, (String, Message))
                                     -> MVar (Map.Map ConversationID (String, Response))
                                     -> e
                                     -> IO
                                          (Stateless.Conversation, MVar Bool,
                                           Chan (ConversationID, (String, Message)),
                                           MVar (Map.Map ConversationID (String, Response)), e)
conversationHandlerChangeHandle handle chan mvar sem = do
    isClosed <- MVar.newEmptyMVar
    MVar.putMVar isClosed False
    forkIO $ whileNotMVar isClosed (do
        Stateless.receiveMessageInternal handle VG.parseConversation (\_ -> return ()) (\mes des -> do
            case des of
                ConversationMessage cid message -> Chan.writeChan chan (cid, (mes, message))
                ConversationResponse cid response -> do
                    mymap <- MVar.takeMVar mvar
                    MVar.putMVar mvar $ Map.insert cid (mes, response) mymap
                ConversationCloseAll -> do
                    Config.traceNetIO $ "Received Message: " ++ mes
                    MVar.takeMVar isClosed
                    MVar.putMVar isClosed True
                    forkIO $ catch (hClose $ fst handle) onException
                    return ()
            )
        )
    return (handle, isClosed, chan, mvar, sem)
    where
        whileNotMVar :: MVar.MVar Bool -> IO () -> IO ()
        whileNotMVar mvar func = do
            shouldStop <- MVar.readMVar mvar
            unless shouldStop (do
                _ <- func
                whileNotMVar mvar func
                )
        onException :: IOException -> IO ()
        onException _ = return ()

receiveResponse :: Conversation -> Int -> Int -> IO (Maybe Response)
receiveResponse conv waitTime tries = do
    responsesMap <- MVar.readMVar $ convRespMap conv
    case Map.lookup (convID conv) responsesMap of
        Just (messages, deserial) -> do
            return $ Just deserial
        Nothing -> do
            if tries /= 0  then do 
                threadDelay waitTime
                receiveResponse conv waitTime $ max (tries-1) (-1) else return Nothing

receiveNewMessage :: Connection -> IO (Conversation, String, Message)
receiveNewMessage connection@(handle, isClosed, chan, mvar, sem) = do
    (cid, (serial, deserial)) <- Chan.readChan chan
    return (Conversation cid handle mvar sem, serial, deserial)

startConversation :: ActiveConnectionsFast -> String -> String -> Int -> Int -> IO (Maybe Conversation)
startConversation = startConversationInternal True

startConversationInternal :: Bool -> ActiveConnectionsFast -> String -> String -> Int -> Int -> IO (Maybe Conversation)
startConversationInternal shouldShowDebug acmvar hostname port waitTime tries = do
    conversationid <- newRandomID
    connectionMap <- MVar.takeMVar acmvar
    case Map.lookup (hostname, port) connectionMap of
        Just (handle, isClosed, chan, mvar, sem) -> do
            handleClosed <- MVar.readMVar isClosed
            if handleClosed then do
                statelessActiveCons <- Stateless.createActiveConnections
                mbyNewHandle <- Stateless.startConversationInternal shouldShowDebug statelessActiveCons hostname port waitTime tries
                case mbyNewHandle of
                    Just handle -> do
                        newconnection@(handle, isClosed, chan, mvar, sem) <- conversationHandlerChangeHandle handle chan mvar sem
                        MVar.putMVar acmvar $ Map.insert (hostname, port) newconnection connectionMap
                        return $ Just (Conversation conversationid handle mvar sem)
                    Nothing -> do
                        MVar.putMVar acmvar connectionMap
                        return Nothing
            else do
                MVar.putMVar acmvar connectionMap
                return $ Just (Conversation conversationid handle mvar sem)
        Nothing -> do
            statelessActiveCons <- Stateless.createActiveConnections
            mbyNewHandle <- Stateless.startConversationInternal shouldShowDebug statelessActiveCons hostname port waitTime tries
            case mbyNewHandle of
                Just handle -> do
                    newconnection@(handle, isClosed, chan, mvar, sem) <- conversationHandler handle
                    MVar.putMVar acmvar $ Map.insert (hostname, port) newconnection connectionMap
                    return $ Just (Conversation conversationid handle mvar sem)
                Nothing -> do
                    MVar.putMVar acmvar connectionMap
                    return Nothing

waitForConversation :: ActiveConnectionsFast -> String -> String -> Int -> Int -> IO (Maybe Conversation)
waitForConversation ac hostname port waitTime tries = do
    Config.traceNetIO $ "Trying to connect to: " ++ hostname ++":"++port
    wFCInternal ac hostname port waitTime tries
    where
        wFCInternal :: ActiveConnectionsFast -> String -> String -> Int -> Int -> IO (Maybe Conversation)
        wFCInternal ac hostname port waitTime tries = do
            mbyConv <- startConversationInternal False ac hostname port waitTime tries
            case mbyConv of
                Just conv -> return mbyConv
                Nothing -> wFCInternal ac hostname port waitTime tries

createActiveConnections :: IO ActiveConnectionsFast
createActiveConnections = do
    activeConnections <- MVar.newEmptyMVar
    MVar.putMVar activeConnections Map.empty
    return activeConnections

acceptConversations :: ActiveConnectionsFast -> ConnectionHandler -> Int -> MVar.MVar (Map.Map Int ServerSocket) -> VChanConnections -> IO ServerSocket
acceptConversations ac connectionhandler port socketsmvar vchanconnections = do
    sockets <- MVar.takeMVar socketsmvar
    case Map.lookup port sockets of
        Just socket -> do
            MVar.putMVar socketsmvar sockets
            return socket
        Nothing -> do
            clientlist <- createServer ac connectionhandler port vchanconnections
            let newsocket = (clientlist, show port)
            let updatedMap = Map.insert port newsocket sockets
            MVar.putMVar socketsmvar updatedMap
            return newsocket
    where
        createServer :: ActiveConnectionsFast -> ConnectionHandler -> Int -> VChanConnections ->  IO (MVar.MVar [(String, (Syntax.Type, Syntax.Type))])
        createServer activeCons connectionhandler port vchanconnections = do
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
            clientlist <- MVar.newEmptyMVar
            MVar.putMVar clientlist []
            forkIO $ acceptClients activeCons connectionhandler vchanconnections clientlist sock $ show port
            return clientlist

        acceptClients :: ActiveConnectionsFast -> ConnectionHandler -> MVar.MVar (Map.Map String (NetworkConnection Value)) -> MVar.MVar [(String, (Syntax.Type, Syntax.Type))] -> Socket -> String -> IO ()
        acceptClients activeCons connectionhandler mvar clientlist socket ownport = do
            clientsocket <- accept socket

            forkIO $ acceptClient activeCons connectionhandler mvar clientlist clientsocket ownport
            acceptClients activeCons connectionhandler mvar clientlist socket ownport

        acceptClient :: ActiveConnectionsFast -> ConnectionHandler -> MVar.MVar (Map.Map String (NetworkConnection Value)) -> MVar.MVar [(String, (Syntax.Type, Syntax.Type))] -> (Socket, SockAddr) -> String -> IO ()
        acceptClient activeCons connectionhandler mvar clientlist clientsocket ownport = do
            hdl <- Stateless.getHandleFromSocket $ fst clientsocket
            let statelessConv = (hdl, clientsocket)
            connection@(handle, isClosed, chan, responsesMvar, sem) <- conversationHandler statelessConv 
            forkIO $ forever (do
                (conversationid, (serial, deserial)) <- Chan.readChan chan
                connectionhandler activeCons mvar clientlist clientsocket (Conversation conversationid statelessConv responsesMvar sem) ownport serial deserial
                )
            return ()

endConversation :: Conversation -> Int -> Int -> IO ()
endConversation _ _ _ = return ()

sayGoodbye :: ActiveConnectionsFast -> IO ()
sayGoodbye activeCons = do
    activeConsMap <- MVar.readMVar activeCons
    let connections = Map.elems activeConsMap
    runAll sayGoodbyeConnection connections
    where
        sayGoodbyeConnection :: Connection -> IO ()
        sayGoodbyeConnection connection@(statelessconv@(handle, _), isClosed, messages, responses, sem) = do
            forkIO $ catch (do
                handleClosed <- MVar.readMVar isClosed
                unless handleClosed $ SSem.withSem sem $ Stateless.sendMessage statelessconv ConversationCloseAll
                unless handleClosed $ SSem.withSem sem $ hPutStr handle " "
                hFlushAll handle
                hClose handle
                ) onException
            return ()
        runAll _ [] = return ()
        runAll f (x:xs) = do
            _ <- f x
            runAll f xs
        onException :: IOException -> IO ()
        onException _ = return ()

getPartnerHostaddress :: Conversation -> String
getPartnerHostaddress = Stateless.getPartnerHostaddress . convHandle
