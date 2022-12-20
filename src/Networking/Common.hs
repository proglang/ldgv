{-# LANGUAGE LambdaCase #-}

module Networking.Common where

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Concurrent
import GHC.IO.Handle
import Control.Monad.IO.Class
import System.IO
import Control.Exception
import qualified Control.Concurrent.Chan as Chan
import qualified Control.Concurrent.MVar as MVar
import ProcessEnvironment

import qualified Networking.Serialize as NSerialize 
import Networking.Messages
import Control.Exception

import qualified ValueParsing.ValueTokens as VT
import qualified ValueParsing.ValueGrammar as VG
import qualified Networking.DirectionalConnection as DC
import Networking.DirectionalConnection (DirectionalConnection)
import Networking.Serialize (Serializable (serialize))
import Networking.NetworkConnection
import qualified Config

newtype ServerException = NoIntroductionException String
    deriving Eq

instance Show ServerException where
    show = \case
        NoIntroductionException s -> "Partner didn't introduce itself, but sent: " ++ s

instance Exception ServerException

sendMessage :: NSerialize.Serializable a => a -> Handle -> IO ()
sendMessage value handle = do
    serializedValue <- NSerialize.serialize value
    -- Config.traceIO $ "Sending message:" ++ serializedValue
    hPutStrLn handle (serializedValue ++" ")

recieveMessage :: Handle -> IO (Maybe Messages)
recieveMessage handle = do
    message <- hGetLine handle
    case VT.runAlex message VG.parseMessages of
    -- case VT.runAlex message VG.parseValues of
        Left err -> do 
            Config.traceIO $ "Error during recieving a networkmessage: "++err
            return Nothing
        Right deserialmessage -> return $ Just deserialmessage

recieveResponse :: Handle -> IO (Maybe Responses)
recieveResponse handle = do
    message <- hGetLine handle
    case VT.runAlex message VG.parseResponses of
        Left err -> do 
            Config.traceIO $ "Error during recieving a networkmessage: "++err
            return Nothing
        Right deserialmessage -> return $ Just deserialmessage

getHandle :: Socket -> IO Handle
getHandle socket = do
    hdl <- socketToHandle socket ReadWriteMode
    hSetBuffering hdl NoBuffering
    return hdl


-- This waits until the handle is established
getClientHandle :: String -> String -> IO Handle
getClientHandle hostname port = do
    catch ( do
        let hints = defaultHints {
                addrFlags = []
              , addrSocketType = Stream
            }   
        addrInfo <- getAddrInfo (Just hints) (Just hostname) $ Just port
        clientsocket <- openSocketNC $ head addrInfo
        connect clientsocket $ addrAddress $ head addrInfo
        getHandle clientsocket) $ expredirect hostname port
    where
        expredirect :: String -> String -> IOException -> IO Handle
        expredirect hostname port e = do
            threadDelay 1000000
            getClientHandle hostname port  

waitForServerIntroduction :: Handle -> IO String
waitForServerIntroduction handle = do
    message <- hGetLine handle
    case VT.runAlex message VG.parseResponses of
        Left err -> do 
            Config.traceIO $ "Error during server introduction: "++err
            throw $ NoIntroductionException message
        Right deserial -> case deserial of
            OkayIntroduce partner -> do
                return partner
            _ -> do 
                Config.traceIO $ "Error during server introduction, wrong message: "++ message
                throw $ NoIntroductionException message

replaceVChan :: Value -> IO Value
replaceVChan input = case input of
    VSend v -> do
        nv <- replaceVChan v
        return $ VSend nv
    VPair v1 v2 -> do 
        nv1 <- replaceVChan v1
        nv2 <- replaceVChan v2
        return $ VPair nv1 nv2
    VFunc penv a b -> do
        newpenv <- replaceVChanPEnv penv
        return $ VFunc newpenv a b
    VDynCast v g -> do 
        nv <- replaceVChan v
        return $ VDynCast nv g
    VFuncCast v a b -> do 
        nv <- replaceVChan v
        return $ VFuncCast nv a b
    VRec penv a b c d -> do 
        newpenv <- replaceVChanPEnv penv
        return $ VRec newpenv a b c d
    VNewNatRec penv a b c d e f g -> do 
        newpenv <- replaceVChanPEnv penv
        return $ VNewNatRec newpenv a b c d e f g
    VChan nc _ _-> do 
        (r, rl, w, wl, pid, oid, h, p) <- serializeNetworkConnection nc
        return $ VChanSerial (r, rl) (w, wl) pid oid (h, p)
    _ -> return input
    where
        replaceVChanPEnv :: [(String, Value)] -> IO [(String, Value)]
        replaceVChanPEnv [] = return []
        replaceVChanPEnv (x:xs) = do 
            newval <- replaceVChan $ snd x
            rest <- replaceVChanPEnv xs
            return $ (fst x, newval):rest

-- openSocket addr = socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
openSocketNC addr = socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)