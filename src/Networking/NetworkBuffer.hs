{-# LANGUAGE LambdaCase #-}

module Networking.NetworkBuffer where

import Networking.Buffer
import Control.Concurrent.MVar
import Control.Exception
import Data.Functor
import qualified Data.Maybe
import Control.Monad
import qualified Control.Concurrent.SSem as SSem

data NetworkBuffer a = NetworkBuffer {buffer :: Buffer a, bufferOffset :: MVar Int, bufferAllMessagesLength :: MVar Int, working :: SSem.SSem}
    deriving Eq

data NetworkBufferSerial a = NetworkBufferSerial {serialList :: [a], serialBufferOffset :: Int, serialBufferAllMessagesLength :: Int}
    deriving (Show, Eq)

type MinimalNetworkBufferSerial a = ([a], Int, Int)

data NetworkBufferException = InvalidAcknowledgementCount Int Int

instance Show NetworkBufferException where
    show = \case
        InvalidAcknowledgementCount found requested -> "NetworkBufferException (InvalidAcknowledgement): Only " ++ show found ++ " values found, out of the requested " ++ show requested ++ " values to acknowledge!"

instance Exception NetworkBufferException

newNetworkBuffer :: IO (NetworkBuffer a)
newNetworkBuffer = do
    buf <- newBuffer
    count <- newMVar 0
    allMessages <- newMVar 0
    work <- SSem.new 1
    return $ NetworkBuffer buf count allMessages work

write :: NetworkBuffer a -> a -> IO Int
write nb value = SSem.withSem (working nb) $ modifyMVar (bufferAllMessagesLength nb) $ \len -> do 
    writeBuffer (buffer nb) value
    return (len+1, len)

writeIfNext :: NetworkBuffer a -> Int -> a -> IO Bool
writeIfNext nb index value = SSem.withSem (working nb) $ modifyMVar (bufferAllMessagesLength nb) $ \len -> if index == len then do
    writeBuffer (buffer nb) value
    return (len+1, True) else return (len, False)

tryGetAtNB :: NetworkBuffer a -> Int -> IO (Maybe a)
tryGetAtNB nb count = SSem.withSem (working nb) $ do  
    offset <- readMVar $ bufferOffset nb
    tryGetAt (buffer nb) (count-offset)

tryTake :: NetworkBuffer a -> IO (Maybe (a, Int))
tryTake nb = SSem.withSem (working nb) $ modifyMVar (bufferOffset nb) (\offset -> do
    mbyTakeValue <- tryTakeBuffer (buffer nb)
    case mbyTakeValue of
        Just value -> return (offset+1, Just (value, offset))
        Nothing -> return (offset, Nothing)
    )

getNextOffset :: NetworkBuffer a -> IO Int
getNextOffset = readMVar . bufferOffset

isAllAcknowledged :: NetworkBuffer a -> IO Bool
isAllAcknowledged nb = do
    mbyBuffer <- tryReadBuffer $ buffer nb
    return $ Data.Maybe.isNothing mbyBuffer

updateAcknowledgements :: NetworkBuffer a -> Int -> IO ()
updateAcknowledgements nb target = SSem.withSem (working nb) $ modifyMVar_ (bufferOffset nb) (\offset -> do
        updateAcknowledgementsInternal (buffer nb) target offset
        return $ target+1
    )
    where
        updateAcknowledgementsInternal :: Buffer a -> Int -> Int -> IO ()
        updateAcknowledgementsInternal buf target current | target < current = return ()
                                                          | otherwise = do
                                                            mbyTake <- tryTakeBuffer buf
                                                            case mbyTake of
                                                                Just _ -> updateAcknowledgementsInternal buf target $ current+1
                                                                Nothing -> throw $ InvalidAcknowledgementCount current target

serialize :: NetworkBuffer a -> IO (NetworkBufferSerial a)
serialize nb = SSem.withSem (working nb) $ do
    list <- writeBufferToList $ buffer nb
    offset <- readMVar $ bufferOffset nb
    allMsgs <- readMVar $ bufferAllMessagesLength nb
    return $ NetworkBufferSerial list offset allMsgs



deserialize :: NetworkBufferSerial a -> IO (NetworkBuffer a)
deserialize nbs@(NetworkBufferSerial list offset allMsgs ) = do
    when (offset<allMsgs) $ throw $ InvalidAcknowledgementCount offset allMsgs
    buffer <- newBuffer
    -- Write all the values to buffer
    foldM_ (\x y -> writeBuffer x y >> return x) buffer list
    bOffset <- newMVar offset
    bAllMsgs <- newMVar allMsgs 
    work <- SSem.new 1
    return $ NetworkBuffer buffer bOffset bAllMsgs work

expandSerial :: MinimalNetworkBufferSerial a -> NetworkBufferSerial a
expandSerial (list, readC, ackC) = NetworkBufferSerial list readC ackC

compressSerial :: NetworkBufferSerial a -> MinimalNetworkBufferSerial a
compressSerial (NetworkBufferSerial list readC ackC) = (list, readC, ackC)

serializeMinimal :: NetworkBuffer a -> IO (MinimalNetworkBufferSerial a)
serializeMinimal nb = serialize nb <&> compressSerial

deserializeMinimal :: MinimalNetworkBufferSerial a -> IO (NetworkBuffer a)
deserializeMinimal = deserialize . expandSerial