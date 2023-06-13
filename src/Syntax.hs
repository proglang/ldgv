{-# LANGUAGE DeriveTraversable #-}
module Syntax where

import Data.List (foldl', sortBy, sort)
import Data.Set (Set)
import Data.Bifunctor (second)
import qualified Data.Set as Set

import Kinds

type Ident = String
type TIdent = String
type Nat = Int

data Exp = Let Ident Exp Exp
         | Math (MathOp Exp)
         | Lit Literal
         | Succ Exp
         | NatRec Exp Exp Ident TIdent Ident Type Exp
         | NewNatRec Ident Ident TIdent Type Exp Ident Exp
         | Var Ident
         | Lam Multiplicity Ident Type Exp
         | Rec Ident Ident Exp Exp
         | App Exp Exp
         | Pair Multiplicity Ident Exp Exp
         | LetPair Ident Ident Exp Exp
         | Fst Exp
         | Snd Exp
         | Fork Exp
         | New Type
         | Send Exp
         | Recv Exp
         | Case Exp [(String, Exp)]
         | Cast Exp Type Type
         -- New types
         | Connect Exp Type Exp Exp  -- Connect URL Port Type
         | Accept Exp Type -- Accept Socket Type
  deriving (Show,Eq)

data MathOp e
  = Add e e
  | Sub e e
  | Mul e e
  | Div e e
  | Neg e
  deriving (Show, Eq, Functor, Foldable, Traversable)

data Literal
  = LInt !Int
  | LNat !Nat
  | LDouble !Double
  | LLab !String
  | LUnit
  | LString !String
  deriving (Show, Eq)

data Type
  = TUnit
  | TInt
  | TDouble
  | TBot
  | TDyn
  | TNat
  | TString
  | TNatLeq Integer
  | TNatRec Exp Type TIdent Type
  | TVar Bool TIdent
  | TAbs Ident Type Type -- abstract a variable to close a type; only use in caches!
  | TName Bool TIdent -- the bool indicates whether the type needs to be dualized
  | TLab [String]
  | TFun Multiplicity Ident Type Type
  | TPair Ident Type Type
  | TSend Ident Type Type
  | TRecv Ident Type Type
  | TCase Exp [(String, Type)]
  | TEqn Exp Exp Type
  | TSingle Ident       -- same value (and type) as ident
  deriving (Show)

dualof :: Type -> Type
dualof (TCase e cases) = TCase e (map (second dualof) cases)
dualof (TSend x t s) = TRecv x t (dualof s)
dualof (TRecv x t s) = TSend x t (dualof s)
dualof (TName b tn) = TName (not b) tn
dualof (TVar b tv) = TVar (not b) tv
dualof (TNatRec e tz y ts) = TNatRec e (dualof tz) y (dualof ts)
dualof t = t

cdualof :: Bool -> Type -> Type
cdualof True ty = dualof ty
cdualof False ty = ty

-- subtyping constraint
data Constraint = Type :<: Type

instance Show Constraint where
  show (t1 :<: t2) = show t1 ++ " :<: " ++ show t2

data SegType = SegSend | SegRecv | SegFun Multiplicity | SegPair
  deriving (Show, Eq)

data TypeSegment = Seg { segType :: SegType
                       , segName :: Ident
                       , segTy   :: Type
                       }
                 deriving (Show, Eq)

headSeg :: Type -> Maybe (TypeSegment, Type)
headSeg (TFun m x ty1 ty2) = Just (Seg (SegFun m) x ty1, ty2)
headSeg (TPair x ty1 ty2) = Just (Seg (SegPair) x ty1, ty2)
headSeg (TSend x ty1 ty2) = Just (Seg SegSend x ty1, ty2)
headSeg (TRecv x ty1 ty2) = Just (Seg SegRecv x ty1, ty2)
headSeg ty = Nothing

segFun :: TypeSegment -> Type -> Type
segFun (Seg (SegFun m) x ty1) = TFun m x ty1
segFun (Seg (SegPair) x ty1) = TPair x ty1
segFun (Seg SegSend x ty1) = TSend x ty1
segFun (Seg SegRecv x ty1) = TRecv x ty1

data Decl = DType TIdent Multiplicity Kind Type
          | DSig Ident Occurrence Type
          | DFun Ident [(Multiplicity, Ident, Type)] Exp (Maybe Type)
          | DSub Type Type
          | DEqv Type Type
          | DSubst Ident Exp Exp
          | DAssume TEnv Decl
          deriving (Show, Eq)


type TEnvEntry = (Ident, (Occurrence, Type))
type TEnv = [TEnvEntry]

demoteTE :: TEnv -> TEnv
demoteTE = map demoteEntry
  where
    demoteEntry (x, (mm, ty)) = (x, (demote mm, ty))

-- kind environments

type KEnvEntry = (Type, Kind)

keKind :: KEnvEntry -> Kind
keKind = snd

keType :: KEnvEntry -> Type
keType = fst

type KEnv = [(TIdent, KEnvEntry)]

-- free expression variables
class Freevars t where
  fv :: t -> Set Ident

instance Freevars Exp where
  fv (Let x e1 e2) = fv e1 <> Set.delete x (fv e2)
  fv (Math m) = fv m
  fv (Lit _) = Set.empty
  fv (Var x) = Set.singleton x
  fv (Lam m x t e) = fv t <> Set.delete x (fv e)
  fv (Rec f x e1 e0) = fv e0 <> Set.delete f (Set.delete x (fv e1))
  fv (App e1 e2) = fv e1 <> fv e2
  fv (Pair m x e1 e2) = fv e1 <> Set.delete x (fv e2)
  fv (LetPair x y e1 e2) = fv e1 <> Set.delete x (Set.delete y (fv e2))
  fv (Fst e1) = fv e1
  fv (Snd e1) = fv e1
  fv (Fork e1) = fv e1
  fv (New ty) = fv ty
  fv (Send e1) = fv e1
  fv (Recv e1) = fv e1
  fv (Connect e0 ty e1 e2) = fv e0 <> fv ty <>fv e1 <> fv e2
  fv (Accept e1 ty) = fv e1 <> fv ty
  fv (Case e cases) = foldl' (<>) (fv e) $ map (fv . snd) cases
  fv (Cast e t1 t2) = fv e
  fv (Succ e) = fv e
  fv (NatRec e ez x t y tyy es) = fv e <> fv ez <> Set.delete x (Set.delete y (fv es)) <> fv tyy
  fv (NewNatRec id1 id2 tid t e1 id3 e2) = fv e1 <> fv e2 -- TODO: This is not correct!

instance Freevars e => Freevars (MathOp e) where
  fv = foldMap fv

instance Freevars Type where
  fv TUnit = Set.empty
  fv TInt = Set.empty
  fv TDouble = Set.empty
  fv TString = Set.empty
  fv TBot = Set.empty
  fv TDyn = Set.empty
  fv (TName b tn) = Set.empty
  fv (TVar b tv) = Set.empty
  fv (TLab labs) = Set.empty
  fv (TFun m x ty1 ty2) = fv ty1 <> Set.delete x (fv ty2)
  fv (TPair x ty1 ty2) = fv ty1 <> Set.delete x (fv ty2)
  fv (TSend x ty1 ty2) = fv ty1 <> Set.delete x (fv ty2)
  fv (TRecv x ty1 ty2) = fv ty1 <> Set.delete x (fv ty2)
  fv (TCase e cases) = foldl' (<>) (fv e) $ map (fv . snd) cases
  fv (TEqn e1 e2 ty) = fv e1 <> fv e2 <> fv ty
  fv (TSingle x) = Set.singleton x
  fv TNat = Set.empty
  fv (TNatLeq _) = Set.empty
  fv (TNatRec e tz y ts) = fv e <> fv tz <> Set.delete y (fv ts)
  fv (TAbs x ty1 ty2) = fv ty1 <> Set.delete x (fv ty2)

instance Freevars TypeSegment where
  fv ts = fv (segTy ts)

instance (Freevars t) => Freevars [t] where
  fv xs = foldl' (<>) Set.empty $ map fv xs

instance (Freevars t1, Freevars t2) => Freevars (t1, t2) where
  fv (x1, x2) = fv x1 <> fv x2

-- substitution
class Substitution t where
  subst :: Ident -> Exp -> t -> t

instance Substitution Exp where
  subst x exp = sb
    where
    sb orig@(Var s) = if x == s then exp else orig
    sb (Let y e1 e2) = Let y (sb e1) (if x == y then e2 else sb e2)
    sb (Math m) = Math (subst x exp m)
    sb (Lit l) = Lit l
    sb (Lam m y ty e1)
      | x /= y = Lam m y (subst x exp ty) (sb e1)
      | otherwise = Lam m y (subst x exp ty) e1
    sb (Rec f y e1 e0) = Rec f y (if x /= y then subst x exp e1 else e1) (subst x exp e0)
    sb (Case e1 cases) = Case (sb e1) [(lll, sb e) | (lll, e) <- cases]
    sb orig@(Cast e t1 t2) = Cast (sb e) t1 t2
    sb (App e1 e2) = App (sb e1) (sb e2)
    sb (Pair m y e1 e2) = Pair m y (sb e1) (if x /= y then sb e2 else e2)
    sb (Fst e1) = Fst (sb e1)
    sb (Snd e1) = Snd (sb e1)
    sb (LetPair y1 y2 e1 e2) = LetPair y1 y2 (sb e1) (if x /= y1 && x /= y2 then sb e2 else e2)
    sb (Fork e1) = Fork (sb e1)
    sb (New t) = New t
    sb (Send e1) = Send (sb e1)
    sb (Recv e1) = Recv (sb e1)
    sb (Connect e0 t e1 e2) = Connect (sb e0) t (sb e1) (sb e2)
    sb (Accept e1 t) = Accept (sb e1) t
    sb (Succ e1) = Succ (sb e1)
    sb (NatRec e ez y t z tyz es) =
      NatRec (sb e) (sb ez) y t z (subst x exp tyz) (if x /= y && x /= z then sb es else es)
    sb orig@(NewNatRec id1 id2 tid t e1 id3 e2) = orig -- TODO: This is not correct!

instance Substitution e => Substitution (MathOp e) where
  subst x = fmap . subst x

instance Substitution Type where
  subst x exp (TCase val cases) =
    TCase (subst x exp val) (map (second(subst x exp)) cases)
  subst x exp (TFun m z ty1 ty2) =
    if x == z then
      TFun m z (subst x exp ty1) ty2
    else
      TFun m z (subst x exp ty1) (subst x exp ty2)
  subst x exp (TPair z ty1 ty2) =
    if x == z then
      TPair z (subst x exp ty1) ty2
    else
      TPair z (subst x exp ty1) (subst x exp ty2)
  subst x exp (TSend z ty1 ty2) =
    if x == z then
      TSend z (subst x exp ty1) ty2
    else
      TSend z (subst x exp ty1) (subst x exp ty2)
  subst x exp (TRecv z ty1 ty2) =
    if x == z then
      TRecv z (subst x exp ty1) ty2
    else
      TRecv z (subst x exp ty1) (subst x exp ty2)
  subst x exp (TEqn ex1 ex2 ty) =
    TEqn (subst x exp ex1) (subst x exp ex2) ty
  subst x exp ty@(TLab labs) =
    ty
  subst x exp ty@TInt =
    ty
  subst x exp ty@TDouble =
    ty
  subst x exp ty@TUnit =
    ty
  subst x exp ty@TBot =
    ty
  subst x exp ty@TDyn =
    ty
  subst x exp ty@TString =
    ty
  -- rationale: a type abbreviation has no free variables
  subst x exp ty@(TName b tid) =
    ty
  subst x exp ty@(TVar b tid) =
    ty
  subst x (Var y) ty@(TSingle z) =
    if x == z then TSingle y else ty
  subst x (Lit (LLab l)) ty@(TSingle z) =
    if x == z then TLab [l] else ty
  subst x exp ty@TNat =
    ty
  subst x exp ty@(TNatLeq _) =
    ty
  subst x exp ty@(TNatRec e tz y ts) =
    TNatRec (subst x exp e) (subst x exp tz) y (if x /= y then subst x exp ts else ts)
  subst x exp (TAbs z ty1 ty2) =
    if x == z then
      TAbs z (subst x exp ty1) ty2
    else
      TAbs z (subst x exp ty1) (subst x exp ty2)
  subst x e t = error $ unlines
    [ "src/Syntax.hs:277: non exhaustive pattern:"
    , mconcat
        [ "  subst "
        , showsPrec 11 x " "
        , showsPrec 11 e " "
        , showsPrec 11 t ""
        ]
    , ""
    , "Try to bind the expression with a `let`."
    ]

-- replace singleton types for x by tyx
single :: Ident -> Type -> Type -> Type
single x tyx ty =
  case ty of
    TSingle y | x == y -> tyx
              | otherwise -> ty
    TFun m y t1 t2 ->
      TFun m y (single x tyx t1) (if x==y then t2 else single x tyx t2)
    TPair y t1 t2 ->
      TPair y (single x tyx t1) (if x==y then t2 else single x tyx t2)
    TSend y t1 t2 ->
      TSend y (single x tyx t1) (if x==y then t2 else single x tyx t2)
    TRecv y t1 t2 ->
      TRecv y (single x tyx t1) (if x==y then t2 else single x tyx t2)
    TCase e lts ->
      TCase e (map (second(single x tyx)) lts)
    TEqn e1 e2 t ->
      TEqn e1 e2 (single x tyx t)
    TUnit -> TUnit
    TInt -> TInt
    TDouble -> TDouble
    TBot -> TBot
    TDyn -> TDyn
    TString -> TString
    TName b i -> TName b i
    TVar b i -> TVar b i
    TLab labs -> TLab labs
    TNat -> TNat
    TNatLeq n -> TNatLeq n
    TNatRec e tz y ts ->
      TNatRec e (single x tyx tz) y (if x==y then ts else single x tyx ts)
    TAbs y t1 t2 ->
      TAbs y (single x tyx t1) (if x==y then t2 else single x tyx t2)


varsupply :: Ident -> [Ident]
varsupply x = x : [ x ++ show n | n <- [(0::Integer)..]]

-- | @freshvar template prohibited_vars@
freshvar :: Ident -> Set Ident -> Ident
freshvar x vars = head [ x' | x' <- varsupply x,  x' `Set.notMember` vars]

-- | equality for types modulo alpha equivalence
instance Eq Type where
  TUnit == TUnit = True
  TInt  == TInt  = True
  TDouble == TDouble = True
  TBot  == TBot  = True
  TDyn == TDyn   = True
  TString == TString = True
  TName b s == TName b' s' = b == b' && s == s'
  TVar b s == TVar b' s' = b == b' && s == s'
  TLab labs == TLab labs'  = sort labs == sort labs'
  TFun m x t1 t2 == TFun n y s1 s2 =
      m == n && t1 == s1 && subst x (Var z) t2 == subst y (Var z) s2
    where
      z = freshvar x $ Set.delete x (fv t2) <> Set.delete y (fv s2)
  TPair x t1 t2 == TPair y s1 s2 =
      t1 == s1 && subst x (Var z) t2 == subst y (Var z) s2
    where
      z = freshvar x $ Set.delete x (fv t2) <> Set.delete y (fv s2)
  TSend x t1 t2 == TSend y s1 s2 =
       t1 == s1 && subst x (Var z) t2 == subst y (Var z) s2
    where
      z = freshvar x $ Set.delete x (fv t2) <> Set.delete y (fv s2)
  TRecv x t1 t2 == TRecv y s1 s2 =
       t1 == s1 && subst x (Var z) t2 == subst y (Var z) s2
    where
      z = freshvar x $ Set.delete x (fv t2) <> Set.delete y (fv s2)
  TCase e cases == TCase e' cases' =
      labs == labs' && e == e' && and (zipWith (==) scases scases')
    where
      cmp x1 x2 = compare (fst x1) (fst x2)
      scases = sortBy cmp cases
      scases' = sortBy cmp cases'
      labs =  map fst scases
      labs' = map fst scases'
  TEqn e1 e2 t == TEqn e1' e2' t' =
      e1 == e1' && e2 == e2' && t == t'
  TSingle x == TSingle y =
    x == y
  TNat == TNat =
    True
  TNatLeq n == TNatLeq n' = n == n'
  TNatRec e tz x ts == TNatRec e' tz' x' ts' =
    e == e' &&
    tz == tz' &&
    tsubst x (TName True z) ts == tsubst x' (TName True z) ts'
    where                       -- TODO: should use free type names
      z = freshvar x $ Set.delete x (fv ts) <> Set.delete x' (fv ts')
  TAbs x t1 t2 == TAbs y s1 s2 =
       t1 == s1 && subst x (Var z) t2 == subst y (Var z) s2
    where
      z = freshvar x $ Set.delete x (fv t2) <> Set.delete y (fv s2)
  -- catchall
  _ == _ = False

-- substitute a type name by a type
tsubst :: TIdent -> Type -> Type -> Type
tsubst tn tyn ty = ts ty
  where
    ts ty =
      case ty of
        TName b ti -> if ti == tn then tyn else ty
        TVar b ti -> ty
        TInt -> TInt
        TDouble -> TDouble
        TBot -> TBot
        TDyn -> TDyn
        TNat -> TNat
        TNatLeq _ -> ty
        TString -> TString
        TNatRec e tz ti tsu ->
          TNatRec e (ts tz) ti (if ti == tn then tsu else ts tsu)
        TLab _ -> ty
        TFun m x t1 t2 -> TFun m x (ts t1) (ts t2)
        TPair x t1 t2 -> TPair x (ts t1) (ts t2)
        TSend x t1 t2 -> TSend x (ts t1) (ts t2)
        TRecv x t1 t2 -> TRecv x (ts t1) (ts t2)
        TCase e cases -> TCase e (map (second ts) cases)
        TEqn e1 e2 t -> TEqn e1 e2 (ts t)
        TSingle x -> ty
        TUnit -> TUnit
        TAbs _ _ _ -> ty
