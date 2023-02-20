{-# LANGUAGE LambdaCase #-}

module Networking.NetworkBuffer where

import Networking.Buffer
import Control.Concurrent.MVar
import Control.Exception
import Data.Functor
import qualified Data.Maybe
import Control.Monad

data NetworkBuffer a = NetworkBuffer {readNetworkBuffer :: Buffer a, readCounter :: MVar Int, acknowledgeNetworkBuffer :: Buffer a, acknowledgeCounter :: MVar Int, writeCounter :: MVar Int}
    deriving Eq

data NetworkBufferSerial a = NetworkBufferSerial {serialList :: [a], serialReadCounter :: Int, serialAcknowledgeCounter :: Int, serialWriteCounter :: Int}
    deriving (Show, Eq)

type MinimalNetworkBufferSerial a = ([a], Int, Int, Int)

data NetworkBufferException = InvalidAcknowledgementCount Int Int

instance Show NetworkBufferException where
    show = \case
        InvalidAcknowledgementCount found requested -> "NetworkBufferException (InvalidAcknowledgement): Only " ++ show found ++ " values found, out of the requested " ++ show requested ++ " values to acknowledge!"

instance Exception NetworkBufferException

newNetworkBuffer :: IO (NetworkBuffer a)
newNetworkBuffer = do
    readBuf <- newBuffer
    readC <- newMVar 0
    acknowledgeBuf <- cloneBuffer readBuf
    acknowledgeC <- newMVar 0
    writeC <- newMVar 0
    return $ NetworkBuffer readBuf readC acknowledgeBuf acknowledgeC writeC

writeNetworkBuffer :: NetworkBuffer a -> a -> IO Int
writeNetworkBuffer nb value = modifyMVar (writeCounter nb) $ \writeCount -> do 
    writeBuffer (readNetworkBuffer nb) value
    return (writeCount+1, writeCount)

writeNetworkBufferIfNext :: NetworkBuffer a -> Int -> a -> IO Bool
writeNetworkBufferIfNext nb count value = modifyMVar (writeCounter nb) $ \writeCount -> do
    if writeCount == count then do 
        writeBuffer (readNetworkBuffer nb) value
        return (writeCount+1, True) else return (writeCount, False)

tryTakeAcknowledgeValue :: NetworkBuffer a -> IO (Maybe (a, Int))
tryTakeAcknowledgeValue nb = modifyMVar (acknowledgeCounter nb) (\acknowledgeCount -> do
    mbyAcknowledgeValue <- tryTakeBuffer $ acknowledgeNetworkBuffer nb
    case mbyAcknowledgeValue of
        Just acknowledgeValue -> return (acknowledgeCount+1, Just (acknowledgeValue, acknowledgeCount+1))
        Nothing -> return (acknowledgeCount, Nothing)
    )

tryTakeReadValue :: NetworkBuffer a -> IO (Maybe a)
tryTakeReadValue nb = modifyMVar (acknowledgeCounter nb) (\acknowledgeCount -> do
    retval <- modifyMVar (readCounter nb) (\readCount ->
        if acknowledgeCount > readCount then (do
            mbyReadValue <- tryTakeBuffer $ readNetworkBuffer nb
            case mbyReadValue of
                Just readValue -> return (readCount+1, Just readValue)
                Nothing -> return (readCount, Nothing)
            )
        else return (readCount, Nothing)
        )
    return (acknowledgeCount, retval)
    )

getRequiredReadValue :: NetworkBuffer a -> IO Int
getRequiredReadValue = readMVar . readCounter

isAllAcknowledged :: NetworkBuffer a -> IO Bool
isAllAcknowledged nb = do
    mbyBuffer <- tryReadBuffer $ acknowledgeNetworkBuffer nb
    return $ Data.Maybe.isNothing mbyBuffer

tryGetReadAt :: NetworkBuffer a -> Int -> IO (Maybe a)
tryGetReadAt nb count = modifyMVar (readCounter nb) (\readCount -> do 
    ret <- tryGetAt (readNetworkBuffer nb) (count-readCount)
    return (readCount, ret)
    )

tryGetAcknowledgeAt :: NetworkBuffer a -> Int -> IO (Maybe a)
tryGetAcknowledgeAt nb count = modifyMVar (acknowledgeCounter nb) (\ackCount -> do
    ret <- tryGetAt (readNetworkBuffer nb) (count-ackCount) 
    return (ackCount, ret)
    )




updateAcknowledgements :: NetworkBuffer a -> Int -> IO Bool
updateAcknowledgements nb target = modifyMVar (acknowledgeCounter nb) (updateAcknowledgementsInternal (acknowledgeNetworkBuffer nb) target)
    where
        updateAcknowledgementsInternal :: Buffer a -> Int -> Int -> IO (Int, Bool)
        updateAcknowledgementsInternal ackBuffer targetCount nbCount = do
            when (nbCount > targetCount) $ throw $ InvalidAcknowledgementCount nbCount targetCount
            if nbCount == targetCount then return (nbCount, True) else do
                mbyTakeBuffer <- tryTakeBuffer ackBuffer
                case mbyTakeBuffer of
                    Just takeBuffer -> updateAcknowledgementsInternal ackBuffer targetCount (nbCount+1)
                    Nothing -> throw $ InvalidAcknowledgementCount nbCount targetCount

serialize :: NetworkBuffer a -> IO (NetworkBufferSerial a)
serialize nb = modifyMVar (writeCounter nb) (\writeCount -> do 
    (bufferList, readCount, acknowledgeCount) <- modifyMVar (acknowledgeCounter nb) (\acknowledgeCount -> do
        (bufferList, readCount) <- modifyMVar (readCounter nb) (\readCount -> do
                bufferList <- writeBufferToList $ readNetworkBuffer nb
                return (readCount, (bufferList, readCount))
            )
        return (acknowledgeCount, (bufferList, readCount, acknowledgeCount))
        )
    return (writeCount, NetworkBufferSerial bufferList readCount acknowledgeCount writeCount)
    )

deserialize :: NetworkBufferSerial a -> IO (NetworkBuffer a)
deserialize nbs@(NetworkBufferSerial list readCount ackCount writeCount) = do
    when (ackCount<readCount) $ throw $ InvalidAcknowledgementCount readCount ackCount
    readB <- newBuffer
    -- Write all the values to buffer
    foldM_ (\x y -> writeBuffer x y >> return x) readB list
    ackB <- cloneBuffer readB
    ackC <- newMVar ackCount
    readC <- newMVar readCount
    writeC <- newMVar writeCount
    -- Drop the values already acknowledged but not read
    forM_ [1..(ackCount-readCount)] $ \_ -> takeBuffer ackB
    return $ NetworkBuffer readB readC ackB ackC writeC

expandSerial :: MinimalNetworkBufferSerial a -> NetworkBufferSerial a
expandSerial (list, readC, ackC, writeC) = NetworkBufferSerial list readC ackC writeC

compressSerial :: NetworkBufferSerial a -> MinimalNetworkBufferSerial a
compressSerial (NetworkBufferSerial list readC ackC writeC) = (list, readC, ackC, writeC)

serializeMinimal :: NetworkBuffer a -> IO (MinimalNetworkBufferSerial a)
serializeMinimal nb = serialize nb <&> compressSerial

deserializeMinimal :: MinimalNetworkBufferSerial a -> IO (NetworkBuffer a)
deserializeMinimal = deserialize . expandSerial