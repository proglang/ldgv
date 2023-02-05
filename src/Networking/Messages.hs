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
    | RequestSync UserID Int
    | IntroduceNewPartnerAddress UserID Port
    deriving Eq

data Responses
    = Redirect Hostname Port
    | Okay
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
    RequestSync p _ -> p
    IntroduceNewPartnerAddress p _ -> p