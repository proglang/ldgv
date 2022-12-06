module Networking.Messages where

import ProcessEnvironment

type Partner = String
type Hostname = String
type Port = String

data Messages
    = Introduce Partner
    | IntroduceClient Partner Port
    | IntroduceServer Partner
    | NewValue Partner Value
    | SyncIncoming Partner [Value]
    | RequestSync Partner
    | ChangePartnerAddress Partner Hostname Port
    deriving Eq
