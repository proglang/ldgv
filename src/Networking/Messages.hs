{-# LANGUAGE LambdaCase #-}

module Networking.Messages where

import ProcessEnvironmentTypes
import Syntax

type UserID = String
type Hostname = String
type Port = String
type ConversationID = String
type ConnectionID = String

data Message
    = Introduce UserID Port Type Type
    | NewValue UserID Int Value
    | RequestValue UserID Int
    | AcknowledgeValue UserID Int
    | NewPartnerAddress UserID Port ConnectionID
    | AcknowledgePartnerAddress UserID ConnectionID
    | Disconnect UserID
    | AcknowledgeDisconnect UserID -- Vielleicht brauchen wir das nicht mal sehen
    deriving Eq

data Response
    = Redirect Hostname Port
    | Okay
    | OkayIntroduce UserID
    | Wait
    | Error

data ConversationSession
    = ConversationMessage ConversationID Message
    | ConversationResponse ConversationID Response
    | ConversationCloseAll


getUserID :: Message -> String
getUserID = \case
    Introduce p _ _ _ -> p
    NewValue p _ _ -> p
    RequestValue p _ -> p
    AcknowledgeValue p _ -> p
    NewPartnerAddress p _ _ -> p
    AcknowledgePartnerAddress p _ -> p 
    Disconnect p -> p
    AcknowledgeDisconnect p -> p
