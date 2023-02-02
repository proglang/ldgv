{-# LANGUAGE LambdaCase #-}

module Networking.Messages where

import ProcessEnvironmentTypes
import Syntax

type UserID = String
type Hostname = String
type Port = String
type ConversationID = String

data Messages
    = IntroduceClient UserID Port Type
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

data ConversationSession
    = ConversationMessage ConversationID Messages
    | ConversationResponse ConversationID Responses
    | ConversationCloseAll

getUserID :: Messages -> String
getUserID = \case
    IntroduceClient p _ _ -> p
    NewValue p _ _ -> p
    SyncIncoming p _ -> p
    RequestSync p -> p
    ChangePartnerAddress p _ _ -> p
    RequestClose p -> p
    IntroduceNewPartnerAddress p _ -> p