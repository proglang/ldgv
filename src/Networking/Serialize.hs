{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module Networking.Serialize where

import Control.Concurrent.Chan as Chan
import qualified Control.Concurrent.MVar as MVar
import Syntax
import Kinds
import Data.Set
import Control.Exception
import ProcessEnvironmentTypes
import Networking.Messages
import qualified Networking.DirectionalConnection as DC
import qualified Networking.NetworkConnection as NCon 
import qualified Data.Maybe
import qualified Networking.DirectionalConnection as NCon


newtype SerializationException = UnserializableException String
    deriving Eq

instance Show SerializationException where
    show = \case
        (UnserializableException s) -> "UnserializableException: " ++ s ++ " is not serializable"

instance Exception SerializationException


class Serializable a where
  serialize :: a -> IO String


instance Serializable ConversationSession where
  serialize = \case
    ConversationMessage c m -> serializeLabeledEntryMulti "NConversationMessage" c $ sLast m
    ConversationResponse c r -> serializeLabeledEntryMulti "NConversationResponse" c $ sLast r
    ConversationCloseAll -> return "NConversationCloseAll"

instance Serializable Response where
  serialize = \case
    Redirect host port -> serializeLabeledEntryMulti "NRedirect" host $ sLast port
    Okay -> return "NOkay"
    OkayIntroduce u -> serializeLabeledEntry "NOkayIntroduce" u
    Wait -> return "NWait"

instance Serializable Message where
  serialize = \case
      IntroduceClient p port tn t -> serializeLabeledEntryMulti "NIntroduceClient" p $ sNext port $ sNext tn $ sLast t
      NewValue p c v -> serializeLabeledEntryMulti "NNewValue" p $ sNext c $ sLast v
      RequestValue p c -> serializeLabeledEntryMulti "NRequestValue" p $ sLast c
      AcknowledgeValue p c -> serializeLabeledEntryMulti "NAcknowledgeValue" p $ sLast c
      NewPartnerAddress p port conID -> serializeLabeledEntryMulti "NNewPartnerAddress" p $ sNext port $ sLast conID
      AcknowledgePartnerAddress p conID -> serializeLabeledEntryMulti "NAcknowledgePartnerAddress" p $ sLast conID
      Disconnect p -> serializeLabeledEntry "NDisconnect" p
      AcknowledgeDisconnect p -> serializeLabeledEntry "NAcknowledgeDisconnect" p

instance Serializable (NCon.NetworkConnection Value) where
  serialize con = do 
    constate <- MVar.readMVar $ NCon.ncConnectionState con
    (readList, readUnread) <- DC.serializeConnection $ NCon.ncRead con
    (writeList, writeUnread) <- DC.serializeConnection $ NCon.ncWrite con

    serializeLabeledEntryMulti "SNetworkConnection" (NCon.ncRead con) $ sNext (NCon.ncWrite con) $ sNext (Data.Maybe.fromMaybe "" $ NCon.ncPartnerUserID con) $ sNext (Data.Maybe.fromMaybe "" $ NCon.ncOwnUserID con) $ sLast constate

instance Serializable (NCon.DirectionalConnection Value) where
  serialize dcon = do
    (msg, msgUnread) <- DC.serializeConnection dcon

    serializeLabeledEntryMulti "SDirectionalConnection" msg $ sLast msgUnread

instance Serializable NCon.ConnectionState where
  serialize = \case
    NCon.Connected hostname port-> serializeLabeledEntryMulti "SConnected" hostname $ sLast port
    _ -> throw $ UnserializableException "VChan can only be serialized when in Connected mode"

instance Serializable Value where
  serialize = \case
      VUnit -> return "VUnit"
      VLabel s -> serializeLabeledEntry "VLabel" s
      VInt i -> serializeLabeledEntry "VInt" i
      VDouble d -> serializeLabeledEntry "VDouble" d
      VString s -> serializeLabeledEntry "VString" s
      VSend v -> serializeLabeledEntry "VSend" v
      VPair a b -> serializeLabeledEntryMulti "VPair" a $ sLast b
      VType t -> serializeLabeledEntry "VType" t
      VFunc env s exp -> serializeLabeledEntryMulti "VFunc" env $ sNext s $ sLast exp
      VDynCast v t -> serializeLabeledEntryMulti "VDynCast" v $ sLast t
      VFuncCast v ft1 ft2 -> serializeLabeledEntryMulti "VFuncCast" v $ sNext ft1 $ sLast ft2
      VRec env f x e0 e1 -> serializeLabeledEntryMulti "VRec" env $ sNext f $ sNext x $ sNext e0 $ sLast e1
      VNewNatRec env f n tid ty ez x es -> serializeLabeledEntryMulti "VNewNatRec" env $ sNext f $ sNext n $ sNext tid $ sNext ty $ sNext ez $ sNext x $ sLast es

      VServerSocket {} -> throw $ UnserializableException "VServerSocket"
      VChan nc _-> serializeLabeledEntry "VChan" nc
      VChanSerial r w p o c -> serializeLabeledEntryMulti "VChanSerial" r $ sNext w $ sNext p $ sNext o $ sLast c

instance Serializable Multiplicity where
  serialize = \case
    MMany -> return "MMany"
    MOne -> return "MOne"

instance Serializable Type where
  serialize = \case
    TUnit -> return "TUnit"
    TInt -> return "TInt"
    TDouble -> return "TDouble"
    TBot -> return "TBot"
    TDyn -> return "TDyn"
    TNat -> return "TNat"
    TString -> return "TString"
    TNatLeq i -> serializeLabeledEntry "TNatLeq" i
    TNatRec e t1 ident t2 -> serializeLabeledEntryMulti "TNatRec" e $ sNext t1 $ sNext ident $ sLast t2
    TVar b ident -> serializeLabeledEntryMulti "TVar" b $ sLast ident
    TAbs ident t1 t2 -> serializeLabeledEntryMulti "TAbs" ident $ sNext t1 $ sLast t2
    TName b ident -> serializeLabeledEntryMulti "TName" b $ sLast ident
    TLab arr -> serializeLabeledEntry "TLab" arr
    TFun mult ident t1 t2 -> serializeLabeledEntryMulti "TFun" mult $ sNext ident $ sNext t1 $ sLast t2
    TPair ident t1 t2 -> serializeLabeledEntryMulti "TPair" ident $ sNext t1 $ sLast t2
    TSend ident t1 t2 -> serializeLabeledEntryMulti "TSend" ident $ sNext t1 $ sLast t2
    TRecv ident t1 t2 -> serializeLabeledEntryMulti "TRecv" ident $ sNext t1 $ sLast t2
    TCase e arr -> serializeLabeledEntryMulti "TCase" e $ sLast arr
    TEqn e1 e2 t -> serializeLabeledEntryMulti "TEqn" e1 $ sNext e2 $ sLast t
    TSingle ident -> serializeLabeledEntry "TSingle" ident

    TServerSocket -> return "TServerSocket"

instance Serializable Exp where
  serialize = \case
    Let ident e1 e2 -> serializeLabeledEntryMulti "ELet" ident $ sNext e1 $ sLast e2
    Math mathop -> serializeLabeledEntry "EMath" mathop
    Lit l -> serializeLabeledEntry "ELit" l
    Succ e -> serializeLabeledEntry "ESucc" e
    NatRec e1 e2 ident1 ident2 ident3 t e3 -> serializeLabeledEntryMulti "NatRec" e1 $ sNext e2 $ sNext ident1 $ sNext ident2 $ sNext ident3 $ sNext t $ sLast e3
    NewNatRec ident1 ident2 ident3 t e1 ident4 e2 -> serializeLabeledEntryMulti "ENewNatRec" ident1 $ sNext ident2 $ sNext ident3 $ sNext t $ sNext e1 $ sNext ident4 $ sLast e2
    Var ident -> serializeLabeledEntry "EVar" ident
    Lam mult ident t e -> serializeLabeledEntryMulti "ELam" mult $ sNext ident $ sNext t $ sLast e
    Rec ident1 ident2 e1 e2 -> serializeLabeledEntryMulti "ERec" ident1 $ sNext ident2 $ sNext e1 $ sLast e2
    App e1 e2 -> serializeLabeledEntryMulti "EApp" e1 $ sLast e2
    Pair mult ident e1 e2 -> serializeLabeledEntryMulti "EPair" mult $ sNext ident $ sNext e1 $ sLast e2
    LetPair ident1 ident2 e1 e2 -> serializeLabeledEntryMulti "ELetPair" ident1 $ sNext ident2 $ sNext e1 $ sLast e2
    Fst e -> serializeLabeledEntry "EFst" e
    Snd e -> serializeLabeledEntry "ESnd" e
    Fork e -> serializeLabeledEntry "EFork" e
    New t -> serializeLabeledEntry "ENew" t
    Send e -> serializeLabeledEntry "ESend" e
    Recv e -> serializeLabeledEntry "ERecv" e
    Case e arr -> serializeLabeledEntryMulti "ECase" e $ sLast arr
    Cast e t1 t2 -> serializeLabeledEntryMulti "ECast" e $ sNext t1 $ sLast t2

    Connect e0 t e1 e2 -> serializeLabeledEntryMulti "EConnect" e0 $ sNext t $ sNext e1 $ sLast e2
    Accept e t -> serializeLabeledEntryMulti "EAccept" e $ sLast t

instance Serializable (MathOp Exp) where
  serialize = \case
    Add e1 e2 -> serializeLabeledEntryMulti "MAdd" e1 $ sLast e2
    Sub e1 e2 -> serializeLabeledEntryMulti "MSub" e1 $ sLast e2
    Mul e1 e2 -> serializeLabeledEntryMulti "MMul" e1 $ sLast e2
    Div e1 e2 -> serializeLabeledEntryMulti "MDiv" e1 $ sLast e2
    Neg e -> serializeLabeledEntry "MNeg" e
instance Serializable Literal where
  serialize = \case
    LInt i -> serializeLabeledEntry "LInt" i
    LNat i -> serializeLabeledEntry "LNat" i
    LDouble d -> serializeLabeledEntry "LDouble" d
    LLab s -> serializeLabeledEntry "LLab" s
    LUnit -> return "LUnit"
    LString s -> serializeLabeledEntry "LString" s

instance Serializable FuncType where
  serialize (FuncType env s t1 t2) = serializeLabeledEntryMulti "SFuncType" env $ sNext s $ sNext t1 $ sLast t2

instance Serializable GType where
  serialize = \case
    GUnit -> return "GUnit"
    GLabel lt -> serializeLabeledEntry "GLabel" lt
    GFunc mult -> serializeLabeledEntry "GFunc" mult
    GPair -> return "GPair"
    GNat -> return "GNat"
    GNatLeq i -> serializeLabeledEntry "GNatLeq" i
    GInt -> return "GInt"
    GDouble -> return "GDouble"
    GString -> return "GString"

sLast :: Serializable a => a -> IO String
sLast x = sNext x $ return ""

sNext :: Serializable a => a -> IO String -> IO String
sNext x ios = do
  xString <- serialize x
  iosString <- ios
  return $ " (" ++ xString ++ ")" ++ iosString

serializeLabeledEntryMulti :: Serializable a => String -> a -> IO String -> IO String
serializeLabeledEntryMulti label x ios = do
    xString <- serialize x
    iosString <- ios
    return $ label ++ " (" ++ xString ++ ")" ++ iosString

serializeLabeledEntry :: Serializable a => String -> a -> IO String
serializeLabeledEntry label x = do
    xString <- serialize x
    return $ label ++ " (" ++ xString ++ ")"

instance {-# OVERLAPPING  #-} Serializable String where
  serialize s = return $ "String:"++ show s

instance Serializable Int where
  serialize i = return $ "Int:" ++ show i

instance Serializable Integer where
  serialize i = return $ "Integer:" ++ show i

instance Serializable Bool where
  serialize b = return $ "Bool:" ++ show b

instance Serializable Double where
  serialize d = return $ "Double:" ++ show d

-- instance (Serializable a => Serializable (Set a)) where 
--   serialize as = "{" ++ serializeElements (elems as) ++ "}"

-- instance {-# OVERLAPPABLE #-} (Serializable a => Serializable [a]) where
--   serialize arr = "["++ serializeElements arr ++"]"

instance ((Serializable a, Serializable b) => Serializable (a, b)) where
  serialize (s, t) = do
    ss <- serialize s
    ts <- serialize t
    return $ "((" ++ ss ++ ") (" ++ ts ++ "))"

instance {-# OVERLAPPING #-} Serializable PEnv where
  serialize arr = serializeLabeledArray "PEnv" arr

instance {-# OVERLAPPING #-} Serializable PEnvEntry where
  serialize (s, t) = do
    ss <- serialize s
    ts <- serialize t
    return $ "PEnvEntry (" ++ ss ++ ") (" ++ ts ++ ")"

instance {-# OVERLAPPING #-} Serializable LabelType where
  serialize as = serializeLabeledArray "SLabelType" (elems as)

instance {-# OVERLAPPING #-} Serializable [(String, Exp)] where
  serialize arr = serializeLabeledArray "SStringExpArray" arr

instance {-# OVERLAPPING #-} Serializable [(String, Type)] where
  serialize arr = serializeLabeledArray "SStringTypeArray" arr

instance {-# OVERLAPPING #-} Serializable [String] where
  serialize arr = serializeLabeledArray "SStringArray" arr

instance {-# OVERLAPPING #-}Serializable [Value] where
  serialize arr = serializeLabeledArray "SValuesArray" arr



instance Serializable (Chan.Chan Value) where
  serialize c = do
    ccnt <- getChanContents c
    serialize ccnt

serializeLabeledArray :: Serializable a => String -> [a] -> IO String
serializeLabeledArray label arr = do
  elems <- serializeElements arr
  return $ label ++ " [" ++ elems ++ "]"

serializeElements :: Serializable a => [a] -> IO String
serializeElements [] = return ""
serializeElements [x] = serialize x
serializeElements (x:xs) = do
  h <- serialize x
  t <- serializeElements xs
  return $ h ++ ", " ++ t