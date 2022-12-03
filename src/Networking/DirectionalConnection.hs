module Networking.DirectionalConnection (DirectionalConnection(..), newConnection, writeMessage, allMessages, readUnreadMessage) where

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

writeMessage :: DirectionalConnection a -> a -> IO ()
writeMessage connection message = do
    modifyMVar_ (messages connection) (\m -> do
        return $ m ++ [message]
        )

-- Gives all outMessages until this point
allMessages :: DirectionalConnection a -> IO [a]
allMessages connection = readMVar (messages connection)

readUnreadMessage :: DirectionalConnection a -> IO a
readUnreadMessage connection = modifyMVar (messagesUnreadStart connection) (\i -> do 
    messagesBind <- allMessages connection
    return ((i+1), (messagesBind!!i))
    )


test = do
    mycon <- newConnection
    writeMessage mycon "a"
    writeMessage mycon "b"
    allMessages mycon >>= print
    readUnreadMessage mycon >>= print
    allMessages mycon >>= print