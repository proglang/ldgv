{-# LANGUAGE LambdaCase #-}
module Networking.Server where

import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.Chan as Chan
import Control.Concurrent (forkIO)
import Control.Monad.IO.Class
import Data.Map
import GHC.IO.Handle
import Network.Socket

import Networking.Messages
import qualified ValueParsing.ValueTokens as VT
import qualified ValueParsing.ValueGrammar as VG
import qualified Networking.Common as NC
import qualified Networking.Serialize as NSerialize

import Control.Exception

newtype ServerException = NoIntroductionException String
    deriving Eq

instance Show ServerException where
    show = \case
        NoIntroductionException s -> "Client didn't introduce itself, but sent: " ++ s

instance Exception ServerException


createServer :: Int -> IO (MVar.MVar (Map String Handle))
createServer port = do
    sock <- liftIO $ socket AF_INET Stream 0
    liftIO $ setSocketOption sock ReuseAddr 1
    let hints = defaultHints {
            addrFlags = [AI_PASSIVE]
          , addrSocketType = Stream
    }
    addrInfo <- liftIO $ getAddrInfo (Just hints) Nothing $ Just $ show port
        
    liftIO $ bind sock $ addrAddress $ head addrInfo
    liftIO $ listen sock 2
    mvar <- MVar.newEmptyMVar
    MVar.putMVar mvar empty
    return mvar

acceptClients :: MVar.MVar (Map String (Handle, SockAddr)) -> Chan.Chan String -> Socket -> IO ()
acceptClients mvar chan socket = do
    clientsocket <- accept socket
    forkIO $ acceptClient mvar chan clientsocket
    acceptClients mvar chan socket


acceptClient :: MVar.MVar (Map String (Handle, SockAddr)) -> Chan.Chan String -> (Socket, SockAddr) -> IO ()
acceptClient mvar chan clientsocket = do
    hdl <- NC.getHandle $ fst clientsocket
    userid <- waitForIntroduction hdl
    MVar.modifyMVar_ mvar (return . insert userid (hdl, snd clientsocket))
    Chan.writeChan chan userid

waitForIntroduction :: Handle -> IO String
waitForIntroduction handle = do
    message <- hGetLine handle
    case VT.runAlex message VG.parseMessages of
        Left err -> do 
            putStrLn $ "Error during client introduction: "++err
            throw $ NoIntroductionException message
        Right deserial -> case deserial of
            Introduce partner -> return partner
            _ -> do 
                putStrLn $ "Error during client introduction, wrong message: "++ message
                throw $ NoIntroductionException message