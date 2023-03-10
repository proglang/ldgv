{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module Networking.Serialize where

import Control.Exception
import Data.Set
import Kinds
import Networking.Messages
import ProcessEnvironmentTypes
import Syntax

newtype SerializationException = UnserializableException String
    deriving Eq

instance Show SerializationException where
    show = \case
        (UnserializableException s) -> "UnserializableException: " ++ s ++ " is not serializable"

instance Exception SerializationException


class Serializable a where
  serialize :: a -> String

class SerializableList b where
  toSer :: String -> b

instance SerializableList String where
  toSer = id

instance (SerializableList b, Serializable a) => SerializableList (a -> b) where
  toSer serList serElem = toSer $ serList ++ "(" ++ serialize serElem ++ ")"

merge :: (SerializableList b) => b
merge = toSer ""

serializeArgs :: (SerializableList b) => b
serializeArgs = toSer ""

instance Serializable ConversationSession where
  serialize = \case
    ConversationMessage c m ->  "NConversationMessage" ++ serializeArgs c  m
    ConversationResponse c r ->  "NConversationResponse" ++ serializeArgs c  r
    ConversationCloseAll ->  "NConversationCloseAll"

instance Serializable Response where
  serialize = \case
    Redirect host port ->  "NRedirect" ++ serializeArgs host  port
    Okay ->  "NOkay"
    OkayIntroduce u ->  "NOkayIntroduce" ++ serializeArgs u
    Wait ->  "NWait"
    Error ->  "NError"

instance Serializable Message where
  serialize = \case
      Introduce p port tn t ->  "NIntroduce" ++ serializeArgs p  port  tn  t
      NewValue p c v ->  "NNewValue" ++ serializeArgs p  c  v
      RequestValue p c ->  "NRequestValue" ++ serializeArgs p  c
      AcknowledgeValue p c ->  "NAcknowledgeValue" ++ serializeArgs p  c
      NewPartnerAddress p port conID ->  "NNewPartnerAddress" ++ serializeArgs p  port  conID
      AcknowledgePartnerAddress p conID ->  "NAcknowledgePartnerAddress" ++ serializeArgs p  conID
      Disconnect p ->  "NDisconnect" ++ serializeArgs p
      AcknowledgeDisconnect p ->  "NAcknowledgeDisconnect" ++ serializeArgs p

instance Serializable Value where
  serialize = \case
      VUnit ->  "VUnit"
      VLabel s ->  "VLabel" ++ serializeArgs s
      VInt i ->  "VInt" ++ serializeArgs i
      VDouble d ->  "VDouble" ++ serializeArgs d
      VString s ->  "VString" ++ serializeArgs s
      VSend v ->  "VSend" ++ serializeArgs v
      VPair a b ->  "VPair" ++ serializeArgs a b
      VType t ->  "VType" ++ serializeArgs t
      VFunc env s exp ->  "VFunc" ++ serializeArgs env  s  exp
      VDynCast v t ->  "VDynCast" ++ serializeArgs v  t
      VFuncCast v ft1 ft2 ->  "VFuncCast" ++ serializeArgs v  ft1  ft2
      VRec env f x e0 e1 ->  "VRec" ++ serializeArgs env f  x  e0  e1
      VNewNatRec env f n tid ty ez x es ->  "VNewNatRec" ++ serializeArgs env  f  n  tid  ty  ez  x  es
      VChan {} -> throw $ UnserializableException "VChan"
      VChanSerial r w p o c ->  "VChanSerial" ++ serializeArgs r  w  p  o  c

instance Serializable Multiplicity where
  serialize = \case
    MMany ->  "MMany"
    MOne ->  "MOne"

instance Serializable Type where
  serialize = \case
    TUnit ->  "TUnit"
    TInt ->  "TInt"
    TDouble ->  "TDouble"
    TBot ->  "TBot"
    TDyn ->  "TDyn"
    TNat ->  "TNat"
    TString ->  "TString"
    TNatLeq i ->  "TNatLeq" ++ serializeArgs i
    TNatRec e t1 ident t2 ->  "TNatRec" ++ serializeArgs e  t1  ident  t2
    TVar b ident ->  "TVar" ++ serializeArgs b  ident
    TAbs ident t1 t2 ->  "TAbs" ++ serializeArgs ident  t1  t2
    TName b ident ->  "TName" ++ serializeArgs b  ident
    TLab arr ->  "TLab" ++ serializeArgs arr
    TFun mult ident t1 t2 ->  "TFun" ++ serializeArgs mult  ident  t1  t2
    TPair ident t1 t2 ->  "TPair" ++ serializeArgs ident  t1  t2
    TSend ident t1 t2 ->  "TSend" ++ serializeArgs ident  t1  t2
    TRecv ident t1 t2 ->  "TRecv" ++ serializeArgs ident  t1  t2
    TCase e arr ->  "TCase" ++ serializeArgs e  arr
    TEqn e1 e2 t ->  "TEqn" ++ serializeArgs e1  e2  t
    TSingle ident ->  "TSingle" ++ serializeArgs ident

instance Serializable Exp where
  serialize = \case
    Let ident e1 e2 ->  "ELet" ++ serializeArgs ident  e1  e2
    Math mathop ->  "EMath" ++ serializeArgs mathop
    Lit l ->  "ELit" ++ serializeArgs l
    Succ e ->  "ESucc" ++ serializeArgs e
    NatRec e1 e2 ident1 ident2 ident3 t e3 ->  "NatRec" ++ serializeArgs e1  e2  ident1  ident2  ident3  t  e3
    NewNatRec ident1 ident2 ident3 t e1 ident4 e2 ->  "ENewNatRec" ++ serializeArgs ident1  ident2  ident3  t  e1  ident4  e2
    Var ident ->  "EVar" ++ serializeArgs ident
    Lam mult ident t e ->  "ELam" ++ serializeArgs mult  ident  t  e
    Rec ident1 ident2 e1 e2 ->  "ERec" ++ serializeArgs ident1  ident2  e1  e2
    App e1 e2 ->  "EApp" ++ serializeArgs e1  e2
    Pair mult ident e1 e2 ->  "EPair" ++ serializeArgs mult  ident  e1  e2
    LetPair ident1 ident2 e1 e2 ->  "ELetPair" ++ serializeArgs ident1  ident2  e1  e2
    Fst e ->  "EFst" ++ serializeArgs e
    Snd e ->  "ESnd" ++ serializeArgs e
    Fork e ->  "EFork" ++ serializeArgs e
    New t ->  "ENew" ++ serializeArgs t
    Send e ->  "ESend" ++ serializeArgs e
    Recv e ->  "ERecv" ++ serializeArgs e
    Case e arr ->  "ECase" ++ serializeArgs e  arr
    Cast e t1 t2 ->  "ECast" ++ serializeArgs e  t1  t2

    Connect e0 t e1 e2 ->  "EConnect" ++ serializeArgs e0  t  e1  e2
    Accept e t ->   "EAccept" ++ serializeArgs e  t

instance Serializable (MathOp Exp) where
  serialize = \case
    Add e1 e2 ->  "MAdd" ++ serializeArgs e1  e2
    Sub e1 e2 ->  "MSub" ++ serializeArgs e1  e2
    Mul e1 e2 ->  "MMul" ++ serializeArgs e1  e2
    Div e1 e2 ->  "MDiv" ++ serializeArgs e1  e2
    Neg e ->  "MNeg" ++ serializeArgs e
instance Serializable Literal where
  serialize = \case
    LInt i ->  "LInt" ++ serializeArgs i
    LNat i ->  "LNat" ++ serializeArgs i
    LDouble d ->  "LDouble" ++ serializeArgs d
    LLab s ->  "LLab" ++ serializeArgs s
    LUnit ->  "LUnit"
    LString s ->  "LString" ++ serializeArgs s

instance Serializable FuncType where
  serialize (FuncType env s t1 t2) = "SFuncType" ++ serializeArgs env  s  t1  t2

instance Serializable GType where
  serialize = \case
    GUnit -> "GUnit"
    GLabel lt -> "GLabel" ++ serializeArgs lt
    GFunc mult -> "GFunc" ++ serializeArgs mult
    GPair -> "GPair"
    GNat -> "GNat"
    GNatLeq i -> "GNatLeq" ++ serializeArgs i
    GInt -> "GInt"
    GDouble -> "GDouble" 
    GString -> "GString"

instance {-# OVERLAPPING  #-} Serializable String where
  serialize s = "String:"++ show s

instance Serializable Int where
  serialize i = "Int:" ++ show i

instance Serializable Integer where
  serialize i = "Integer:" ++ show i

instance Serializable Bool where
  serialize b = "Bool:" ++ show b

instance Serializable Double where
  serialize d = "Double:" ++ show d

instance ((Serializable a, Serializable b) => Serializable (a, b)) where
  serialize (s, t) = "((" ++ serialize s++ ") (" ++ serialize t ++ "))"

instance ((Serializable a, Serializable b, Serializable c) => Serializable (a, b, c)) where
  serialize (s, t, u) = "((" ++ serialize s ++ ") (" ++ serialize t ++ ") (" ++ serialize u ++ "))"

instance ((Serializable a, Serializable b, Serializable c, Serializable d) => Serializable (a, b, c, d)) where
  serialize (s, t, u, v) = "((" ++ serialize s ++ ") (" ++ serialize t ++ ") (" ++ serialize u ++ ") (" ++ serialize v ++ "))"

instance {-# OVERLAPPING #-} Serializable PEnv where
  serialize arr = serializeLabeledArray "PEnv" arr

instance {-# OVERLAPPING #-} Serializable PEnvEntry where
  serialize (s, t) = "PEnvEntry (" ++ serialize s ++ ") (" ++ serialize t ++ ")"

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

serializeLabeledArray :: Serializable a => String -> [a] ->  String
serializeLabeledArray label arr = label ++ " [" ++ serializeElements arr ++ "]"

serializeElements :: Serializable a => [a] ->  String
serializeElements [] = ""
serializeElements [x] = serialize x
serializeElements (x:xs) = serialize x ++ ", " ++ serializeElements xs
