module Networking.DirectionalConnection (DirectionalConnection(..)) where

import Control.Concurrent.Chan
import Control.Concurrent.MVar

data DirectionalConnection a = DirectionalConnection { messagesAll :: Chan a, messagesUnread :: Chan a, messagesUnreadStart :: MVar Int, messagesEnd :: MVar Int}
    deriving Eq


newConnection :: IO (DirectionalConnection a)
newConnection = do
    messagesAll <- newChan
    messagesUnread <- dupChan messagesAll
    messagesUnreadStart <- newEmptyMVar 
    putMVar messagesUnreadStart 0
    messagesEnd <- newEmptyMVar 
    putMVar messagesEnd 0
    return $ DirectionalConnection messagesAll messagesUnread messagesUnreadStart messagesEnd

writeMessage :: DirectionalConnection a -> a -> IO ()
writeMessage connection message = do
    writeChan (messagesAll connection) message  -- We only need to write it to one channel, since we duplicated them
    modifyMVar_ (messagesEnd connection) (\i -> return $ i+1)

-- Gives all outMessages until this point
allMessages :: DirectionalConnection a -> IO [a]
allMessages connection = do
    messagesEnd <- readMVar $ messagesEnd connection
    messagesDup <- dupChan $ messagesAll connection
    giveMessages messagesDup messagesEnd
    where
        giveMessages :: Chan a -> Int -> IO [a] 
        giveMessages messages 0 = return []
        giveMessages messages count = do 
            x <- readChan messages
            xs <- giveMessages messages $ count-1
            return (x:xs)

readUnreadMessage :: DirectionalConnection a -> IO a
readUnreadMessage connection = do
    modifyMVar_ (messagesUnreadStart connection) (\i -> return $ i+1)
    readChan $ messagesUnread connection



