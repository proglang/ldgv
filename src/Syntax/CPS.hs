{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall #-}
module Syntax.CPS
  ( Val(..)
  , Exp(..)
  , Continuation
  , toCPS
  ) where

import Control.Monad.Cont
import Control.Monad.Reader
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
  | Call Val Val (Maybe Continuation)
    -- ^ Represents a tail call if no continuation is given.
  | Case Val [(String, Exp)]
  | NatRec Val Val Ident TIdent Ident Type (Maybe Continuation) Exp
  | LetCont {-ContIdent-} Continuation Exp
  | Recv Val (Maybe Continuation)
    -- ^ Represents a "tail recv" if no continuation is given
  deriving (Show, Eq)

type Continuation = (Ident, Exp)

toCPS :: S.Exp -> Exp
toCPS e = flip runReader (fv e) $ fromExp' e

fromExp :: S.Exp -> (Val -> Reader (Set Ident) Exp) -> Reader (Set Ident) Exp
fromExp e = runContT (fromExpC e)

fromExpC :: S.Exp -> ContT Exp (Reader (Set Ident)) Val
fromExpC = \case
  S.Lit l -> pure (Lit l)
  S.Var v -> pure (Var v)
  S.Lam m x t e -> lift $
    Lam m x t <$> bound x (fromExp' e)
  S.App e1 e2 -> do
    v1 <- fromExpC e1
    v2 <- fromExpC e2
    captured $ pure . Call v1 v2 . Just
  S.Let x e1 e2 -> do
    v1 <- fromExpC e1
    ContT $ fmap (Let x v1) . bound x . fromExp e2
  S.Rec f x xt rt e -> lift $
    Rec f x xt rt <$> bound2 f x (fromExp' e)
  S.Pair _ x e1 e2 -> do
    v1 <- fromExpC e1
    ContT \k -> Let x v1 <$> bound x do
      fromExp e2 $ k . Pair (Var x)
  S.LetPair x1 x2 e1 e2 -> do
    v1 <- fromExpC e1
    ContT $ fmap (LetPair x1 x2 v1) . bound2 x1 x2 . fromExp e2
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
      c n tyv x t (Just k) <$> bound2 n x (fromExp' s)
  S.Fork e -> do
    vars <- ask
    pure $ Fork $ flip runReader vars $ fromExp e (pure . Return)
  S.New  t -> pure $ New t
  S.Send e -> Send <$> fromExpC e
  S.Recv e -> do
    v <- fromExpC e
    captured $ pure . Recv v . Just

getPair :: (forall a. (a, a) -> a) -> S.Exp -> ContT Exp (Reader (Set Ident)) Val
getPair f e = do
  v <- fromExpC e
  ContT \k -> do
    vars <- ask
    let xfst = freshvar "fst" vars
        xsnd = freshvar "snd" vars
        x = Var $ f (xfst, xsnd)
    LetPair xfst xsnd v <$> bound2 xfst xsnd (k x)

fromExp' :: S.Exp -> Reader (Set Ident) Exp
fromExp' = \case
  S.App e1 e2 -> flip runContT pure do
    Call <$> fromExpC e1 <*> fromExpC e2 <*> pure Nothing
  S.Let x e1 e2 -> fromExp e1 \v ->
    Let x v <$> bound x (fromExp' e2)
  S.Case e cs -> fromExp e \v ->
    Case v <$> traverse (traverse fromExp') cs
  S.NatRec e z n tyv x t s -> flip runContT pure do
    c <- NatRec <$> fromExpC e <*> fromExpC z
    lift $ c n tyv x t Nothing <$> bound2 n x (fromExp' s)
  S.LetPair x1 x2 e1 e2 -> fromExp e1 \v ->
    LetPair x1 x2 v <$> bound2 x1 x2 (fromExp' e2)
  S.Recv e -> fromExp e \v ->
    pure $ Recv v Nothing

  e@S.Lit{}  -> trivial e
  e@S.Var{}  -> trivial e
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

captured :: (Continuation -> Reader (Set Ident) Exp) -> ContT Exp (Reader (Set Ident)) Val
captured f = do
  vars <- ask
  let a = freshvar "a" vars
  ContT \k -> f . (a,) =<< bound a (k (Var a))

bound :: MonadReader (Set Ident) m => Ident -> m a -> m a
bound x = local (Set.insert x)

bound2 :: MonadReader (Set Ident) m => Ident -> Ident -> m a -> m a
bound2 x y = local (Set.insert x . Set.insert y)
