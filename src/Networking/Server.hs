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
import qualified Networking.UserID as UserID
import ProcessEnvironment

import Control.Exception
import qualified Networking.UserID as UserID
import qualified Networking.Messages as Messages
import qualified Networking.DirectionalConnection as ND


createServer :: Int -> IO (MVar.MVar (Map String ConnectionInfo), Chan.Chan String, String)
createServer port = do
    serverid <- UserID.newRandomUserID
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
    chan <- Chan.newChan
    forkIO $ acceptClients mvar chan sock serverid
    return (mvar, chan, serverid)

acceptClients :: MVar.MVar (Map String ConnectionInfo) -> Chan.Chan String -> Socket -> String-> IO ()
acceptClients mvar chan socket serverid = do
    putStrLn "Waiting for clients"
    clientsocket <- accept socket
    putStrLn "Accepted new client"

    forkIO $ acceptClient mvar chan clientsocket serverid
    acceptClients mvar chan socket serverid


acceptClient :: MVar.MVar (Map String ConnectionInfo) -> Chan.Chan String -> (Socket, SockAddr) -> String -> IO ()
acceptClient mvar chan clientsocket serverid = do
    hdl <- NC.getHandle $ fst clientsocket
    userid <- waitForIntroduction hdl serverid
    r <- ND.newConnection
    w <- ND.newConnection
    MVar.modifyMVar_ mvar (return . insert userid (ConnectionInfo hdl (snd clientsocket) r w))
    forkIO $ NC.recieveMessagesID r mvar userid
    Chan.writeChan chan userid

waitForIntroduction :: Handle -> String -> IO String
waitForIntroduction handle serverid = do
    message <- hGetLine handle
    case VT.runAlex message VG.parseMessages of
        Left err -> do 
            putStrLn $ "Error during client introduction: "++err
            throw $ NC.NoIntroductionException message
        Right deserial -> case deserial of
            Introduce partner -> do
                NC.sendMessage (Messages.Introduce serverid) handle
                return partner
            _ -> do 
                putStrLn $ "Error during client introduction, wrong message: "++ message
                throw $ NC.NoIntroductionException message