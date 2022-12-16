{-# LANGUAGE LambdaCase #-}

module Networking.Messages where

import ProcessEnvironment
import Syntax

type Partner = String
type Hostname = String
type Port = String

-- I need to add the Port to every introduction so I can answer oder alles muss mit einem okay quitiert werden, dann kann die antwort gesendet werden
data Messages
    = Introduce Partner
    | IntroduceClient Partner Port Type
    | IntroduceServer Partner
    | NewValue Partner Value
    | SyncIncoming Partner [Value]
    | RequestSync Partner
    | ChangePartnerAddress Partner Hostname Port
    deriving Eq

data Responses
    = Redirect Hostname Port
    | Okay

getPartnerID :: Messages -> String
getPartnerID = \case
    Introduce p -> p
    IntroduceClient p _ _ -> p
    IntroduceServer p -> p
    NewValue p _ -> p
    SyncIncoming p _ -> p
    RequestSync p -> p
    ChangePartnerAddress p _ _ -> p



