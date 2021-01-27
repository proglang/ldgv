{-# LANGUAGE LambdaCase #-}
module Typing where

import Control.Monad (foldM, zipWithM)

import Syntax
import Kinds
import Subtyping

-- kind synthesis
kiSynth :: TEnv -> Type -> Maybe (Kind, Multiplicity)
kiSynth te TUnit = return (Kunit, MMany)
kiSynth te TInt  = return (Kun, MMany)
kiSynth te TBot  = return (Kunit, MMany) -- this kind is compatible with everything
kiSynth te (TLab labs) = do
  (l1 : _) <- return labs
  return (Kidx, MMany)
kiSynth te (TFun m x ty1 ty2) = do
  (k1, m1) <- kiSynth te ty1
  (k2, m2) <- kiSynth ((x, (demote m1, ty1)) : te) ty2
  return (kindof m, m) -- TODO add multiplicity
kiSynth te ty@(TPair m x ty1 ty2) = do
  (k1, m1) <- kiSynth te ty1
  (k2, m2) <- kiSynth ((x, (demote m1, ty1)) : te) ty2
  let mout = max m1 m2
  if mout <= m then return (kindof mout, mout) else fail ("kiSynth " ++ show ty)
kiSynth te (TSend x ty1 ty2) = do
  (k1, m1) <- kiSynth te ty1
  m2 <- kiCheck ((x, (demote m1, ty1)) : te) ty2 Kssn 
  return (Kssn, MOne)
kiSynth te (TRecv x ty1 ty2) = do
  (k1, m1) <- kiSynth te ty1
  m2 <- kiCheck ((x, (demote m1, ty1)) : te) ty2 Kssn 
  return (Kssn, MOne)
kiSynth te (TCase e1 cases) = do
  -- alternative: synthesize the type of e1 and only check the cases arising from this type
  let tlabs = TLab (map fst cases)
  _ <- tyCheck te e1 tlabs
  ks <- mapM (\(lab, elab) -> 
              kiSynth (("*kis*", (Many, TEqn e1 (Lit $ LLab lab) tlabs)) : te) elab)
             cases
  return (foldr1 kolub ks)

-- kind checking
kiCheck :: TEnv -> Type -> Kind -> Maybe ()
kiCheck te ty ki = do
  (k1, m1) <- kiSynth te ty
  if klub ki k1 == ki then Just () else Nothing

-- unrestricted strengthening of top entry
strengthenTop :: TEnv -> Maybe TEnv
strengthenTop ((_, (Zero, _)) : te) = return te
strengthenTop ((_, (Many, _)) : te) = return te
strengthenTop ((_, (One, _)) : te) = Nothing

-- type synthesis
tySynth :: TEnv -> Exp -> Maybe (Type, TEnv)
tySynth te e = case e of
  Let x e1 e2 -> do
    (ty1, te1) <- tySynth te e1
    (ki1, mm) <- kiSynth (demoteTE te1) ty1
    (ty2, te2) <- tySynth ((x, (inject mm, ty1)) : te1) e2
    te2' <- strengthenTop te2
    return (ty2, te2')
  Math m -> tySynthMath te m
  Lit l -> return (tySynthLit l, te)
  Var x -> do
    (mm, tyx) <- lookup x te
    mm' <- use mm
    let te' = map upd te
        upd entry@(y, _) = if x == y then (x, (mm', tyx)) else entry
    return (tyx, te')
  Lam mm x tyx e1 -> do
    (kix, mx) <- kiSynth (demoteTE te) tyx
    (ty, te1) <- tySynth ((x, (inject mx, tyx)) : te) e1
    te1' <- strengthenTop te1
    return (TFun mm x tyx ty, te1')
  Rec f x tyx tyr e1 -> do -- return type is declared
    (kix, mx) <- kiSynth (demoteTE te) tyx
    te1 <- tyCheck ((f, (Many, TFun MMany x tyx tyr)) : (x, (inject mx, tyx)) : te) e1 tyr
    te1' <- strengthenTop te1
    te1'' <- strengthenTop te1'
    return (TFun MMany x tyx tyr, te1'')
  App e1 e2 -> do
    (TFun m x ty1 ty2, te1) <- tySynth te e1
    te2 <- tyCheck te1 e2 ty1
    let tyout = subst x e2 ty2
    _ <- kiSynth (demoteTE te) tyout
    return (tyout, te2)
  Pair mul x e1 e2 -> do
    (ty1, te1) <- tySynth te e1
    (ki1, mu1) <- kiSynth (demoteTE te) ty1
    (ty2, te2) <- tySynth (("*asi*", (demote mu1, TEqn (Var x) e1 ty1)) :
                           (x, (demote mu1, ty1)) :
                           te1) e2
    (ki2, mu2) <- kiSynth (demoteTE te) ty2
    te2' <- strengthenTop te2
    te2'' <- strengthenTop te2'
    return (TPair mul x ty1 ty2, te2'')
  LetPair x y e1 e2 -> do
    (TPair mp z ty1 ty2, te1) <- tySynth te e1
    (ki1, mu1) <- kiSynth (demoteTE te) ty1
    (ki2, mu2) <- kiSynth ((z, (demote mu1, ty1)) : demoteTE te) ty2
    (ty3, te2) <- tySynth ((y, (inject mu2, ty2)) : (x, (inject mu1, ty1)) : te) e2
    (ki3, mu3) <- kiSynth (demoteTE te) ty3
    te2' <- strengthenTop te2
    te2'' <- strengthenTop te2'
    return (ty3, te2'')
  Fst e1 -> do                  -- only if second component can be discarded
    (TPair mp z ty1 ty2, te1) <- tySynth te e1
    (ki1, mu1) <- kiSynth (demoteTE te) ty1
    (ki2, MMany) <- kiSynth ((z, (demote mu1, ty1)) : demoteTE te) ty2
    return (ty1, te1)
  Snd e1 -> do                  -- only if first component can be discarded
    (TPair mp z ty1 ty2, te1) <- tySynth te e1
    (ki1, MMany) <- kiSynth (demoteTE te) ty1
    let ty2' = subst z (Fst e1) ty1
    (ki2, mu2) <- kiSynth (demoteTE te) ty2'
    return (ty2', te1)
  Fork e1 -> do
    te1 <- tyCheck te e1 TUnit
    return (TUnit, te1)
  New ty -> do
    kiCheck (demoteTE te) ty Kssn
    return (TPair MOne "" ty (dualof ty), te)
  Send e1 -> do
    (TSend x ty1 ty2, te1) <- tySynth te e1
    return (TFun MOne x ty1 ty2, te1) -- TODO: needs to be linear
  Recv e1 -> do
    (TRecv x ty1 ty2, te1) <- tySynth te e1
    return (TPair MOne x ty1 ty2, te1)
  Case e1 cases
    | Just (Lit (LLab lab), TLab labels) <- valueEquiv (demoteTE te) e1 ->
      do elab <- lookup lab cases
         tySynth te elab
  Case e1@(Var x) cases -> do
    let labels = map fst cases
        tlabels = TLab labels
    tyCheck (demoteTE te) e1 tlabels
    let flab (lab, elab) = do
          (tylab, telab) <- tySynth (("*lab-e2*", (Many, TEqn e1 (Lit $ LLab lab) tlabels)) : te) elab
          telab' <- strengthenTop telab -- drop the equation
          return (TCase e1 ((lab, tylab) : [(lab', TBot) | lab' <- labels, lab' /= lab]),
                  telab')
    ty_te_s <- mapM flab cases
    ty' <- lubn (demoteTE te) (map fst ty_te_s)
    te' <- tenvJoinN (map snd ty_te_s)
    return (ty', te')
  Case _ _ ->
    fail "illegal case expression"

tySynthLit :: Literal -> Type
tySynthLit = \case
  LInt _ -> TInt
  LNat _ -> TNat
  LLab l -> TLab [l]
  LUnit  -> TUnit

tySynthMath :: TEnv -> MathOp Exp -> Maybe (Type, TEnv)
tySynthMath te m = do
  te' <- foldM (\te' e -> tyCheck te' e TInt) te m
  return (TInt, te')

tyCheck :: TEnv -> Exp -> Type -> Maybe TEnv
tyCheck te e ty = do
  (tysyn, te') <- tySynth te e
  ki <- subtype (demoteTE te) tysyn ty
  return te'

tenvJoinN :: [TEnv] -> Maybe TEnv
tenvJoinN [te] = return te
tenvJoinN (te1:te2:tes) = do
  te12 <- tenvJoin te1 te1
  tenvJoinN (te12:tes)

tenvJoin :: TEnv -> TEnv -> Maybe TEnv
tenvJoin = zipWithM tenvEntryJoin

tenvEntryJoin :: TEnvEntry -> TEnvEntry -> Maybe TEnvEntry
tenvEntryJoin e1@(x1, (m1, ty1)) (x2, (m2, ty2)) 
  | x1 == x2 && m1 == m2 && ty1 == ty2 = return e1
tenvEntryJoin _ _ = Nothing
