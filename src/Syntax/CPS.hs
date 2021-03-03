{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall #-}
module Syntax.CPS
  ( toCPS

    -- * CPS Types
  , Val(..)
  , Exp(..)
  , Continuation

    -- * Re-exports from "Syntax"
  , Ident
  , TIdent
  , Literal(..)
  , MathOp(..)
  , Type(..)
  , Freevars(..)
  ) where

import Control.Monad.Cont
import Control.Monad.Reader
import Data.Foldable
import Data.Functor
import Data.Set (Set)
import Kinds (Multiplicity)
import Syntax hiding (Exp(..))
import qualified Data.Set as Set
import qualified Syntax as S

data Val
  = Lit Literal
  | Var Ident
  | Lam Multiplicity Ident Type {-ContIdent-} Exp
  | Rec Ident Ident Type Type Exp
  | Math (MathOp Val)
  | Succ Val
  | Pair Val Val
  | New Type
  | Send Val
  | Fork Exp
  deriving (Show, Eq)

data Exp
  = Return {-ContIdent-} Val
  | Let Ident Val Exp
  | LetPair Ident Ident Val Exp
  | LetCont {-ContIdent-} Continuation Exp
  | Call Val Val (Maybe Continuation)
    -- ^ Represents a tail call if no continuation is given.
  | TLCall Ident (Maybe Continuation)
    -- ^ A call to a top level function.
  | Case Val [(String, Exp)]
  | NatRec Val Val Ident TIdent Ident Type Exp
  | Recv Val (Maybe Continuation)
    -- ^ Represents a "tail recv" if no continuation is given
  deriving (Show, Eq)

type Continuation = (Ident, Exp)

instance Freevars Val where
  fv = \case
    Lit _ -> Set.empty
    Var v -> Set.singleton v
    Lam _ x t e -> fv t <> Set.delete x (fv e)
    Rec f x t1 t2 e -> fv t1 <> fv t2 <> Set.delete f (Set.delete x (fv e))
    Math m -> fv m
    Succ e -> fv e
    Pair e1 e2 -> fv e1 <> fv e2
    New ty -> fv ty
    Send e -> fv e
    Fork e -> fv e

instance Freevars Exp where
  fv = \case
    Return v -> fv v
    Let x v e -> fv v <> Set.delete x (fv e)
    LetPair x y v e -> fv v <> Set.delete x (Set.delete y (fv e))
    LetCont k e -> contFV k <> fv e
    Call a b mk -> fv a <> fv b <> foldMap contFV mk
    TLCall x mk ->
      -- FIXME: Should x really be considered a free variable, or implicitly
      -- bound by the top level definitions?
      Set.insert x (foldMap contFV mk)
    Case v cs -> foldl' (<>) (fv v) $ map (fv . snd) cs
    NatRec v z x _t y tyy s -> mconcat
      [ fv v
      , fv z
      , Set.delete x (Set.delete y (fv s))
      , fv tyy
      ]
    Recv v mk -> fv v <> foldMap contFV mk
    where
      contFV (x, k) = Set.delete x (fv k)

-- | Keeps track of the used variables in an expression. @varsUsed@ should
-- always be a superset of @varsBound@. The former includes all initially free
-- variables while the latter is used to keep track of all bound variables.
--
-- @varsUsed@ is the set of identifiers we shouldn't use when creating fresh
-- variables, @varsBound@ is used to distinguish used variables between local
-- value accesses and top level calls.
data Vars = Vars
  { varsUsed :: !(Set Ident)
  , varsBound :: !(Set Ident)
  }
  deriving (Show)

toCPS :: S.Exp -> Exp
toCPS e = flip runReader vars $ fromExp' e
  where
    vars = Vars
      { varsUsed = fv e
      , varsBound = mempty
      }

fromExp :: S.Exp -> (Val -> Reader Vars Exp) -> Reader Vars Exp
fromExp e = runContT (fromExpC e)

fromExpC :: S.Exp -> ContT Exp (Reader Vars) Val
fromExpC = \case
  S.Lit l -> pure (Lit l)
  S.Var v -> do
    vBound <- isBound v
    if vBound
       then pure (Var v)
       else captured $ pure . TLCall v . Just
  S.Lam m x t e -> lift $
    Lam m x t <$> bound x (fromExp' e)
  S.App e1 e2 -> do
    v1 <- fromExpC e1
    v2 <- fromExpC e2
    captured $ pure . Call v1 v2 . Just
  S.Let x e1 e2 -> do
    v1 <- fromExpC e1
    (x', e2') <- renaming x e2
    ContT $ fmap (Let x' v1) . bound x' . fromExp e2'
  S.Rec f x xt rt e -> lift $
    Rec f x xt rt <$> bound2 f x (fromExp' e)
  S.Pair _ x e1 e2 -> do
    v1 <- fromExpC e1
    (x', e2') <- renaming x e2
    ContT \k -> Let x' v1 <$> bound x' do
      fromExp e2' $ k . Pair (Var x')
  S.LetPair x1 x2 e1 e2 -> do
    v1 <- fromExpC e1
    (x1', e2a) <- renaming x1 e2
    (x2', e2b) <- renaming x2 e2a
    ContT $ fmap (LetPair x1' x2' v1) . bound2 x1' x2' . fromExp e2b
  S.Fst e -> getPair fst e
  S.Snd e -> getPair snd e
  S.Case e cs -> captured \k ->
    LetCont k <$> fromExp e \v ->
      Case v <$> traverse (traverse fromExp') cs
  S.Math m -> Math <$> traverse fromExpC m
  S.Succ e -> Succ <$> fromExpC e
  S.NatRec e z n tyv x t s -> do
    c <- NatRec <$> fromExpC e <*> fromExpC z
    captured \k ->
      LetCont k . c n tyv x t <$> bound2 n x (fromExp' s)
  S.Fork e -> do
    vars <- ask
    pure $ Fork $ flip runReader vars $ fromExp e (pure . Return)
  S.New  t -> pure $ New t
  S.Send e -> Send <$> fromExpC e
  S.Recv e -> do
    v <- fromExpC e
    captured $ pure . Recv v . Just

getPair :: (forall a. (a, a) -> a) -> S.Exp -> ContT Exp (Reader Vars) Val
getPair f e = do
  v <- fromExpC e
  ContT \k -> do
    xfst <- fresh "fst"
    xsnd <- fresh "snd"
    let x = Var $ f (xfst, xsnd)
    LetPair xfst xsnd v <$> bound2 xfst xsnd (k x)

fromExp' :: S.Exp -> Reader Vars Exp
fromExp' = \case
  S.Var v -> do
    vBound <- isBound v
    if vBound
       then trivial (S.Var v)
       else pure $ TLCall v Nothing
  S.App e1 e2 -> flip runContT pure do
    Call <$> fromExpC e1 <*> fromExpC e2 <*> pure Nothing
  S.Let x e1 e2 -> fromExp e1 \v ->
    Let x v <$> bound x (fromExp' e2)
  S.Case e cs -> fromExp e \v ->
    Case v <$> traverse (traverse fromExp') cs
  S.NatRec e z n tyv x t s -> flip runContT pure do
    c <- NatRec <$> fromExpC e <*> fromExpC z
    lift $ c n tyv x t <$> bound2 n x (fromExp' s)
  S.LetPair x1 x2 e1 e2 -> fromExp e1 \v ->
    LetPair x1 x2 v <$> bound2 x1 x2 (fromExp' e2)
  S.Recv e -> fromExp e \v ->
    pure $ Recv v Nothing

  e@S.Lit{}  -> trivial e
  e@S.Succ{} -> trivial e
  e@S.Math{} -> trivial e
  e@S.Fst{}  -> trivial e
  e@S.Snd{}  -> trivial e
  e@S.Lam{}  -> trivial e
  e@S.Rec{}  -> trivial e
  e@S.Pair{} -> trivial e
  e@S.Fork{} -> trivial e
  e@S.New{}  -> trivial e
  e@S.Send{} -> trivial e
  where
    trivial e = fromExp e (pure . Return)

captured :: (Continuation -> Reader Vars Exp) -> ContT Exp (Reader Vars) Val
captured f = do
  a <- fresh "a"
  ContT \k -> f . (a,) =<< bound a (k (Var a))

bound :: MonadReader Vars m => Ident -> m a -> m a
bound x = local \(Vars v w) -> Vars (Set.insert x v) (Set.insert x w)

bound2 :: MonadReader Vars m => Ident -> Ident -> m a -> m a
bound2 x y = bound x . bound y

isBound :: MonadReader Vars m => Ident -> m Bool
isBound ident = do
  vars <- ask
  pure $ ident `Set.member` varsBound vars

fresh :: MonadReader Vars m => Ident -> m Ident
fresh ident = asks $ freshvar ident . varsUsed

renaming :: MonadReader Vars m => Ident -> S.Exp -> m (Ident, S.Exp)
renaming x e = fresh x <&> \x' ->
  (x', if x == x' then e else subst x (S.Var x') e)
