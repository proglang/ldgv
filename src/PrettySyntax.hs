{-# LANGUAGE LambdaCase #-}
module PrettySyntax (Pretty(), pretty, pshow) where

import Kinds
import Syntax

import Data.Text.Prettyprint.Doc

pshow x = show (pretty x)

instance Pretty Constraint where
  pretty (t1 :<: t2) = pretty t1 <+> pretty "<:" <+> pretty t2

instance Pretty Multiplicity where
  pretty MMany = mempty
  pretty MOne = pretty "!"

instance Pretty Occurrence where
  pretty Many = pretty "_"
  pretty One  = pretty '1'
  pretty Zero = pretty '0'

instance Pretty Kind where
  pretty k = pretty (show k)

instance Pretty TypeSegment where
  pretty (Seg SegSend x t) = pretty "!" <> ptyped x t
  pretty (Seg SegRecv x t) = pretty "?" <> ptyped x t
  pretty (Seg (SegFun m) x t) = pretty "Pi" <> pretty m <> ptyped x t
  pretty (Seg (SegPair m) x t) = pretty "Sg" <> pretty m <> ptyped x t


plab str =
  -- pretty "'" <> -- seem built into the lab string
  pretty str

ptyped ('#':_) t1 =
  pretty t1 <> dot
ptyped id t1 =
  parens (pretty id <+> colon <+> pretty t1)  

instance Pretty Type where
  pretty TUnit = pretty "()"
  pretty TInt = pretty "Int"
  pretty TNat = pretty "Nat"
  pretty TBot = pretty "_|_"
    -- the bool indicates whether the type needs to be dualized
  pretty (TName b s) = (if b then pretty "~" else mempty) <> pretty s
  pretty (TVar b s) = (if b then pretty "~" else mempty) <> brackets (pretty s)
  pretty (TLab (str:strs)) = braces (plab str <> foldr f mempty strs)
    where 
      f str rest = comma <+> plab str <> rest
  pretty (TFun m id t1 t2) =
    pretty m <> ptyped id t1 <+> pretty "->" <+> pretty t2
  pretty (TPair m id t1 t2) =
    brackets (pretty m <> pretty id <+> colon <+> pretty t1 <> comma <+> pretty t2)
  pretty (TSend id t1 t2) =
    pretty "!" <> ptyped id t1 <+> pretty t2
  pretty (TRecv id t1 t2) = 
    pretty "?" <> ptyped id t1 <+> pretty t2
  pretty (TCase e (st : sts)) =
    pcase e (st : sts)
  pretty (TEqn e1 e2 t) =
    pretty "{{" <> pretty e1 <+> equals <> equals <+> pretty e2 <+> colon <+> pretty t <> pretty "}}"
  pretty (TSingle x) =
    pretty "S" <> parens (pretty x)
  pretty (TNatRec e tz y ts) =
    pretty "natrec" <+> pretty e <+>
    braces (pretty "Z:" <+> pretty tz <> comma <+>
           pretty "S_:" <+> pretty y <+> dot <+> pretty ts)
  -- print as a telescope
  pretty (TAbs id t1 t2) =
    ptyped id t1 <+> pretty t2

pcase e (st : sts) = 
  pretty "case" <+> pretty e <+>
  braces (g st <> foldr f mempty sts)
    where g (s, t) = plab s <> colon <> pretty t
          f st rest = comma <+> g st <> rest

instance Pretty Exp where
  pretty (Let id e1 e2) =
    pretty "let" <+> pretty id <+> equals <+> pretty e1 <+> pretty "in" <+> 
    pretty e2
  pretty (Var id) =
    pretty id
  pretty (Lit l) =
    pretty l
  pretty (Math m) =
    pretty m
  pretty (Lam m id t e) =
    pretty "fun" <> pretty m <+> ptyped id t <+>
    pretty e
  pretty (Rec f x t1 t2 e) =
    pretty "rec" <+> pretty f <+> ptyped x t1 <>
    colon <> pretty t2 <+> equals <+> pretty e
  pretty (App e1 e2) =
    pretty e1 <+> pretty e2
  pretty (Pair m id e1 e2) = 
    angles (pretty m <> pretty id <+> equals <+> pretty e1 <> comma <+> pretty e2)
  pretty (LetPair x y e1 e2) =
    pretty "let" <+> angles (pretty x <> comma <> pretty y) <+> equals <+> pretty e1 <+> pretty "in" <+> pretty e2
  pretty (Fst e) = pretty "fst" <+> pretty e
  pretty (Snd e) = pretty "snd" <+> pretty e
  pretty (Fork e) = pretty "fork" <+> pretty e
  pretty (New t) = pretty "new" <+> pretty t
  pretty (Send e) = pretty "send" <+> pretty e
  pretty (Recv e) = pretty "recv" <+> pretty e
  pretty (Case e ses) = 
    pcase e ses
  pretty (Succ e) =
    pretty "succ" <+> pretty e
  pretty (NatRec e ez x t y tyy es) =
    pretty "natrec" <+> pretty e <+>
    braces (pretty ez <> comma <+>
           pretty x <> dot <+>
           pretty t <> dot <+> ptyped y tyy <> dot <+> pretty es)

instance Pretty Literal where
  pretty = \case
    LInt i -> pretty i
    LNat n -> pretty n
    LLab l -> plab l
    LUnit  -> pretty "()"

instance Pretty e => Pretty (MathOp e) where
  pretty = \case
    Add a b -> pretty a <+> pretty "+" <+> pretty b
    Sub a b -> pretty a <+> pretty "-" <+> pretty b
    Mul a b -> pretty a <+> pretty "*" <+> pretty b
    Div a b -> pretty a <+> pretty "/" <+> pretty b
    Neg a   -> pretty "-" <> pretty a
