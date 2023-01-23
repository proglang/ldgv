module Networking.DirectionalConnection (DirectionalConnection(..), newConnection, createConnection, writeMessage, writeMessageIfNext, countMessages, allMessages, readUnreadMessage, readUnreadMessageMaybe, serializeConnection, syncMessages) where

import Control.Concurrent.MVar

data DirectionalConnection a = DirectionalConnection { messages :: MVar [a], messagesUnreadStart :: MVar Int, messagesCount :: MVar Int}
    deriving Eq

-- When a channel is duplicated there are no unread messages in the new channel, only the old one

newConnection :: IO (DirectionalConnection a)
newConnection = do
    messages <- newEmptyMVar
    putMVar messages []
    messagesUnreadStart <- newEmptyMVar 
    putMVar messagesUnreadStart 0
    messagesCount <- newEmptyMVar 
    putMVar messagesCount 0
    return $ DirectionalConnection messages messagesUnreadStart messagesCount


createConnection :: [a] -> Int -> IO (DirectionalConnection a)
createConnection messages unreadStart = do
    msg <- newEmptyMVar
    putMVar msg messages
    messagesUnreadStart <- newEmptyMVar 
    putMVar messagesUnreadStart unreadStart
    messagesCount <- newEmptyMVar 
    putMVar messagesCount $ length messages
    return $ DirectionalConnection msg messagesUnreadStart messagesCount


writeMessage :: DirectionalConnection a -> a -> IO ()
writeMessage connection message = do
    modifyMVar_ (messagesCount connection) (\c -> do
        modifyMVar_ (messages connection) (\m -> return $ m ++ [message])
        return $ c + 1
        )

writeMessageIfNext :: DirectionalConnection a -> Int -> a -> IO Bool
writeMessageIfNext connection count message = do
    modifyMVar (messagesCount connection) (\c ->
        if count == c + 1 then do 
            modifyMVar_ (messages connection) (\m -> return $ m ++ [message])
            return (c + 1, True) 
        else 
            return (c, False)
        )
    

-- This relies on the message array giving having the same first entrys as the internal messages
syncMessages :: DirectionalConnection a -> [a] -> IO ()
syncMessages connection msgs = do
    mymessages <- takeMVar $ messages connection
    if length mymessages < length msgs then putMVar (messages connection) msgs else putMVar (messages connection) mymessages

-- Gives all outMessages until this point
allMessages :: DirectionalConnection a -> IO [a]
allMessages connection = readMVar (messages connection)

readUnreadMessageMaybe :: DirectionalConnection a -> IO (Maybe a)
readUnreadMessageMaybe connection = modifyMVar (messagesUnreadStart connection) (\i -> do 
    messagesBind <- allMessages connection
    if length messagesBind == i then return (i, Nothing) else return ((i+1), Just (messagesBind!!i))
    )
    
readUnreadMessage :: DirectionalConnection a -> IO a
readUnreadMessage connection = do 
    maybeval <- readUnreadMessageMaybe connection
    case maybeval of
        Nothing -> readUnreadMessage connection
        Just val -> return val


serializeConnection :: DirectionalConnection a -> IO ([a], Int)
serializeConnection connection = do
    messageList <- allMessages connection
    messageUnread <- readMVar $ messagesUnreadStart connection
    return (messageList, messageUnread)

countMessages :: DirectionalConnection a -> IO Int
countMessages connection = readMVar $ messagesCount connection

test = do
    mycon <- newConnection
    writeMessage mycon "a"
    writeMessage mycon "b"
    allMessages mycon >>= print
    readUnreadMessage mycon >>= print
    allMessages mycon >>= print
    readUnreadMessage mycon >>= print
    readUnreadMessage mycon >>= print