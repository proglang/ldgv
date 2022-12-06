module Networking.Messages where

import ProcessEnvironment

type Partner = String
type Hostname = String
type Port = String

data Messages
    = Introduce Partner
    | NewValue Partner Value
    | SyncIncoming Partner [Value]
    | RequestSync Partner
    | ChangePartnerAddress Partner Hostname Port
