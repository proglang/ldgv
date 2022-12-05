module Networking.DirectionalConnection (DirectionalConnection(..), newConnection, createConnection, writeMessage, allMessages, readUnreadMessage, readUnreadMessageMaybe) where

import Control.Concurrent.MVar

data DirectionalConnection a = DirectionalConnection { messages :: MVar [a], messagesUnreadStart :: MVar Int}
    deriving Eq

-- When a channel is duplicated there are no unread messages in the new channel, only the old one

newConnection :: IO (DirectionalConnection a)
newConnection = do
    messages <- newEmptyMVar
    putMVar messages []
    messagesUnreadStart <- newEmptyMVar 
    putMVar messagesUnreadStart 0
    return $ DirectionalConnection messages messagesUnreadStart


createConnection :: [a] -> Int -> IO (DirectionalConnection a)
createConnection messages unreadStart = do
    msg <- newEmptyMVar
    putMVar msg messages
    messagesUnreadStart <- newEmptyMVar 
    putMVar messagesUnreadStart unreadStart
    return $ DirectionalConnection msg messagesUnreadStart


writeMessage :: DirectionalConnection a -> a -> IO ()
writeMessage connection message = do
    modifyMVar_ (messages connection) (\m -> do
        return $ m ++ [message]
        )

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


test = do
    mycon <- newConnection
    writeMessage mycon "a"
    writeMessage mycon "b"
    allMessages mycon >>= print
    readUnreadMessage mycon >>= print
    allMessages mycon >>= print
    readUnreadMessage mycon >>= print
    readUnreadMessage mycon >>= print