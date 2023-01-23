{-# LANGUAGE LambdaCase #-}

module Networking.Messages where

import ProcessEnvironment
import Syntax

type UserID = String
type Hostname = String
type Port = String

-- I need to add the Port to every introduction so I can answer oder alles muss mit einem okay quitiert werden, dann kann die antwort gesendet werden
data Messages
    = Introduce UserID
    | IntroduceClient UserID Port Type
    | IntroduceServer UserID
    | NewValue UserID Int Value
    | SyncIncoming UserID [Value]
    | RequestSync UserID
    | ChangePartnerAddress UserID Hostname Port
    | IntroduceNewPartnerAddress UserID Port
    | RequestClose UserID
    deriving Eq

data Responses
    = Redirect Hostname Port
    | Okay
    | OkayClose
    | OkayIntroduce UserID
    | OkaySync [Value]
    | Wait

getUserID :: Messages -> String
getUserID = \case
    Introduce p -> p
    IntroduceClient p _ _ -> p
    IntroduceServer p -> p
    NewValue p _ _ -> p
    SyncIncoming p _ -> p
    RequestSync p -> p
    ChangePartnerAddress p _ _ -> p
    RequestClose p -> p
    IntroduceNewPartnerAddress p _ -> p