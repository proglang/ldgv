module Networking.Messages where

import ProcessEnvironment

type Partner = String
type Hostname = String
type Port = Int

data Message 
    = NewValue Partner Value
    | SyncIncomming Partner [Value]
    | RequestSync Partner
    | ChangePartnerAddress Partner Hostname Port 
