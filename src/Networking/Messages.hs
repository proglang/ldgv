module Networking.Messages where

import ProcessEnvironment
import Syntax

type Partner = String
type Hostname = String
type Port = String

data Messages
    = Introduce Partner
    | IntroduceClient Partner Port Type
    | IntroduceServer Partner
    | NewValue Partner Value
    | SyncIncoming Partner [Value]
    | RequestSync Partner
    | ChangePartnerAddress Partner Hostname Port
    deriving Eq
