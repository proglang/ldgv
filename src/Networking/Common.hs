{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Networking.Common where

-- import qualified Networking.NetworkingMethod.Stateless as NetMethod
import qualified Networking.NetworkingMethod.Fast as NetMethod


type ConversationOrHandle = NetMethod.Conversation 

-- type ConversationOrHandle = (Handle, (Socket, SockAddr)) 

-- The compiler sadly compains when these things get eta reduced :/
sendMessage con ser = NetMethod.sendMessage con ser

sendResponse con ser = NetMethod.sendResponse con ser

startConversation activeCons host port waitTime tries = NetMethod.startConversation activeCons host port waitTime tries

waitForConversation activeCons host port waitTime tries = NetMethod.waitForConversation activeCons host port waitTime tries

createActiveConnections = NetMethod.createActiveConnections

acceptConversations activeCons connectionhandler port socketsmvar = NetMethod.acceptConversations activeCons connectionhandler port socketsmvar

receiveResponse con waitTime tries = NetMethod.receiveResponse con waitTime tries

endConversation con waitTime tries = NetMethod.endConversation con waitTime tries

sayGoodbye con = NetMethod.sayGoodbye con

getPartnerHostaddress conv = NetMethod.getPartnerHostaddress conv
