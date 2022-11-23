module Networking.Server where

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Control.Concurrent
import GHC.IO.Handle
import System.IO
import qualified Control.Concurrent.Chan as Chan
import ProcessEnvironment


import qualified ValueParsing.ValueTokens as VT
import qualified ValueParsing.ValueGrammar as VG
import qualified SerializeValues as SV

-- communicate :: Chan.Chan Value -> Chan.Chan Value -> Socket -> IO ()
communicate read write socket = do
    hdl <- socketToHandle socket ReadWriteMode
    hSetBuffering hdl NoBuffering
    forkIO (sendWritten write hdl)
    recieveReadable read hdl
    where
        sendWritten write handle = do
            message <- readChan write
            hPutStrLn handle (SV.serialize message ++" ")
            sendWritten write handle

        recieveReadable read handle = do
            message <- hGetLine handle
            case VT.runAlex message VG.parseValues of
                Left err -> putStrLn $ "Error during recieving a networkmessage: "++err
                Right deserial -> writeChan read deserial
            recieveReadable read handle

