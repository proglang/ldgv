{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module SerializeValues where

import ProcessEnvironment
import Syntax
import Kinds
import qualified Syntax as S
import Data.Set

class Serializable a where
  serialize :: a -> String

instance Serializable Value where
  serialize = \case
      VUnit -> "VUnit"
      VLabel s -> "VLabel (" ++ serialize s ++ ")"
      VInt i -> "VInt (" ++ serialize i ++ ")"
      VDouble d -> "VDouble (" ++ serialize d ++ ")"
      VString s -> "VString (" ++ serialize s ++ ")"
      VChan _ _ -> "VChan"
      VSend v -> "VSend (" ++ serialize v ++ ")"
      VPair a b -> "VPair (" ++ serialize a ++ ") (" ++ serialize b ++ ")"
      VType t -> "VType (" ++ serialize t ++ ")"
      VFunc env s exp -> "VFunc (" ++ serialize env ++ ") (" ++ serialize s ++ ") (" ++ serialize exp++")"
      VDynCast v t -> "VDynCast (" ++ serialize v ++ ") (" ++ serialize t ++ ")"
      VFuncCast v ft1 ft2 -> "VFuncCast (" ++ serialize v ++ ") (" ++ serialize ft1 ++ ") (" ++ serialize ft2 ++ ")"
      VRec env f x e1 e0 -> "VRec (" ++ serialize env ++") (" ++  serialize f ++ ") (" ++ serialize x ++ ") (" ++ serialize e1 ++ ") (" ++ serialize e0 ++ ")"
      VNewNatRec env f n tid ty ez x es -> "VNewNatRec (" ++ serialize env ++ ") (" ++ serialize f ++ ") (" ++ serialize n ++ ") (" ++ serialize tid ++ 
                                          ") (" ++ serialize ty ++ ") (" ++ serialize ez ++ ") (" ++ serialize x ++ ") ("++ serialize es++ ")"

instance Serializable Type where
  serialize = \case
    TUnit -> "TUnit"
    TInt -> "TInt"
    TDouble -> "TDouble"
    TBot -> "TBot"
    TDyn -> "TDyn"
    TNat -> "TNat"
    TString -> "TString"
    TNatLeq i -> "TNatLeq (" ++ serialize i ++ ")"
    TNatRec e t1 ident t2 -> "TNatRec (" ++ serialize e ++") (" ++ serialize t1 ++ ") (" ++ serialize ident ++ ") (" ++ serialize t2 ++ ")"
    TVar b ident -> "TVar (" ++ serialize b ++ ") (" ++ serialize ident ++ ")"
    TAbs ident t1 t2 -> "TAbs (" ++ serialize ident ++ ") (" ++ serialize t1 ++ ") (" ++ serialize t2 ++ ")"
    TName b ident -> "TName (" ++ serialize b ++ ") (" ++ serialize ident ++ ")"
    TLab arr -> "TLab (" ++ serialize arr ++ ")"
    TFun mult ident t1 t2 -> "TFun (" ++ show mult ++ ") (" ++ serialize ident ++ ") (" ++ serialize t1 ++ ") (" ++ serialize t2 ++ ")"
    TPair ident t1 t2 -> "TPair (" ++ serialize ident ++ ") (" ++ serialize t1 ++ ") (" ++ serialize t2 ++ ")"
    TSend ident t1 t2 -> "TSend (" ++ serialize ident ++ ") (" ++ serialize t1 ++ ") (" ++ serialize t2 ++ ")"
    TRecv ident t1 t2 -> "TRecv (" ++ serialize ident ++ ") (" ++ serialize t1 ++ ") (" ++ serialize t2 ++ ")"
    TCase e arr -> "TCase (" ++ serialize e ++ ") (" ++ serialize arr ++ ")"
    TEqn e1 e2 t -> "TEqn (" ++ serialize e1 ++ ") (" ++ serialize e2 ++ ") (" ++ serialize t ++ ")"
    TSingle ident -> "TSingle (" ++ serialize ident ++ ")"

instance Serializable Exp where
  serialize = \case
    Let ident e1 e2 -> "ELet (" ++ serialize ident ++ ") (" ++ serialize e1 ++ ") (" ++ serialize e2 ++ ")"
    Math mathop -> "EMath (" ++ serialize mathop ++ ")"
    Lit l -> "ELit (" ++ serialize l ++ ")"
    Succ e -> "ESucc (" ++ serialize e ++ ")"
    NatRec e1 e2 ident1 ident2 ident3 t e3 -> "ENatRec (" ++ serialize e1 ++ ") (" ++ serialize e2 ++ ") (" ++ serialize ident1 ++ 
                                              ") (" ++ serialize ident2 ++ ") (" ++ serialize ident3 ++ ") (" ++ serialize t ++ 
                                              ") (" ++ serialize e3 ++ ")"
    NewNatRec ident1 ident2 ident3 t e1 ident4 e2 -> "ENetNatRec (" ++ serialize ident1 ++ ") (" ++ serialize ident2 ++ 
                                                    ") (" ++ serialize ident3++ ") (" ++ serialize t ++ ") (" ++ serialize e1 ++ 
                                                    ") (" ++ serialize ident4 ++ ") (" ++ serialize e2 ++ ")"
    Var ident -> "EVar (" ++ serialize ident ++ ")"
    Lam mult ident t e -> "ELam (" ++ show mult ++ ") (" ++ serialize ident ++ ") (" ++ serialize t ++ ") (" ++ serialize e ++ ")"
    Rec ident1 ident2 e1 e2 -> "ERec (" ++ serialize ident1 ++ ") (" ++ serialize ident2 ++ ") (" ++ serialize e1 ++ 
                              ") (" ++ serialize e2 ++ ")"
    App e1 e2 -> "EApp (" ++ serialize e1 ++ ") (" ++ serialize e2 ++ ")"
    Pair mult ident e1 e2 -> "EPair (" ++ show mult ++ ") (" ++ serialize ident ++ ") (" ++ serialize e1 ++ 
                              ") (" ++ serialize e2 ++ ")"
    LetPair ident1 ident2 e1 e2 -> "ELetPair (" ++ serialize ident1 ++ ") (" ++ serialize ident2 ++ ") (" ++ serialize e1 ++ 
                                  ") (" ++ serialize e2 ++ ")"
    Fst e -> "EFst (" ++ serialize e ++ ")"
    Snd e -> "ESnd (" ++ serialize e ++ ")"
    Fork e -> "EFork (" ++ serialize e ++ ")"
    New t -> "ENew (" ++ serialize t ++ ")"
    Send e -> "ESend (" ++ serialize e ++ ")"
    Recv e -> "ERecv (" ++ serialize e ++ ")"
    Case e arr -> "ECase (" ++ serialize e ++ ") (" ++ serialize arr ++ ")"
    Cast e t1 t2 -> "ECast (" ++ serialize e ++ ") (" ++ serialize t1 ++ ") (" ++ serialize t2 ++ ")"

instance Serializable (MathOp Exp) where
  serialize = \case
    Add e1 e2 -> "MAdd (" ++ serialize e1 ++ ") (" ++ serialize e2 ++ ")"
    Sub e1 e2 -> "MSub (" ++ serialize e1 ++ ") (" ++ serialize e2 ++ ")"
    Mul e1 e2 -> "MMul (" ++ serialize e1 ++ ") (" ++ serialize e2 ++ ")"
    Div e1 e2 -> "MDiv (" ++ serialize e1 ++ ") (" ++ serialize e2 ++ ")"
    Neg e -> "MNeg (" ++ serialize e ++ ")"

instance Serializable Literal where
  serialize = \case
    LInt i -> "LInt (" ++ serialize i ++ ")"
    LNat i -> "LNat (" ++ serialize i ++ ")"
    LDouble d -> "LDouble (" ++ serialize d ++ ")"
    LLab s -> "LLab (" ++ serialize s ++ ")"
    LUnit -> "LUnit"
    LString s -> "LString (" ++ serialize s ++ ")"

instance Serializable FuncType where
  serialize (FuncType env s t1 t2) = "SFuncType (" ++ serialize env ++ ") (" ++ serialize s ++ ") (" ++ serialize t1 ++ ") (" ++ serialize t2 ++ ")"

instance Serializable GType where
  serialize = \case
    GUnit -> "GUnit"
    GLabel lt -> "GLabel (" ++ serialize lt ++ ")"
    GFunc mult -> "GFunc (" ++ show mult ++ ")"
    GPair -> "GPair"
    GNat -> "GNat"
    GNatLeq i -> "GNatLeq (" ++ serialize i ++ ")"
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

instance (Serializable a => Serializable (Set a)) where 
  serialize as = "{" ++ serializeElements (elems as) ++ "}"

instance {-# OVERLAPPABLE #-} (Serializable a => Serializable [a]) where
  serialize arr = "["++ serializeElements arr ++"]"

instance ((Serializable a, Serializable b) => Serializable (a, b)) where
  serialize (s, t) = "((" ++ serialize s ++ ") (" ++ serialize t ++ "))"

instance {-# OVERLAPPING #-} Serializable PEnv where
  serialize arr = "PEnv ["++ serializeElements arr ++"]"

instance {-# OVERLAPPING #-} Serializable PEnvEntry where
  serialize (s, t) = "PEnvEntry (" ++ serialize s ++ ") (" ++ serialize t ++ ")"

instance {-# OVERLAPPING #-} Serializable LabelType where
  serialize as = "SLabelType {" ++ serializeElements (elems as) ++ "}"

instance {-# OVERLAPPING #-} Serializable [(String, Exp)] where
  serialize arr = "SStringExpArray [" ++ serializeElements arr ++ "]"

instance {-# OVERLAPPING #-} Serializable [(String, Type)] where
  serialize arr = "SStringTypeArray [" ++ serializeElements arr ++ "]"

instance {-# OVERLAPPING #-} Serializable [String] where
  serialize arr = "SStringArray [" ++ serializeElements arr ++ "]"

serializeElements :: Serializable a => [a] -> String
serializeElements [] = ""
serializeElements [x] = serialize x
serializeElements (x:xs) = serialize x ++ ", " ++ serializeElements xs 