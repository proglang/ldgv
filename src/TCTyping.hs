{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module TCTyping where

import Control.Monad (foldM, zipWithM, ap)
import qualified Data.Set as Set

import qualified Config as D

import Syntax
import Syntax.Pretty
import Kinds
import TCSubtyping
import qualified TCXMonad as TC
import qualified TCSubtyping as TC

-- type TCM a = TC.M KEnv a

-- kind synthesis
kiSynth :: TEnv -> Type -> TCM (Kind, Multiplicity)
kiSynth te (TName b tn) = do
  kentry <- TC.mlookup tn
  let k = keKind kentry
  return (k, mult k)
kiSynth te TUnit = return (Kunit, MMany)
kiSynth te TInt  = return (Kun, MMany)
kiSynth te TNat  = return (Kun, MMany)
kiSynth te TBot  = return (Kunit, MMany) -- this kind is compatible with everything
kiSynth te (TLab labs) = do
  case labs of
    (_ : _) -> return (Kidx, MMany)
    _ -> TC.mfail "Empty label type"
kiSynth te (TFun m x ty1 ty2) = do
  (k1, m1) <- kiSynth te ty1
  (k2, m2) <- kiSynth ((x, (demote m1, ty1)) : te) ty2
  return (kindof m, m)
kiSynth te ty@(TPair m x ty1 ty2) = do
  (k1, m1) <- kiSynth te ty1
  (k2, m2) <- kiSynth ((x, (demote m1, ty1)) : te) ty2
  let mout = max m1 m2
  if mout <= m then return (kindof mout, mout) else TC.mfail ("kiSynth " ++ pshow ty)
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
kiSynth te (TSingle x) = do
  (mm, tyx) <- maybe (TC.mfail ("No variable " ++ show x)) return $ lookup x te
  kiSynth te tyx
kiSynth te (TNatRec e1 tz y ts) = do
  _ <- tyCheck te e1 TNat
  (kz, mz) <- kiSynth te tz
  TC.mlocal y (TName False y, kz) (kiSynth te ts)
kiSynth te (TVar b v) = do
  kentry <- TC.mlookup v
  let k = keKind kentry
  return (k, mult k)
kiSynth te ty =
  TC.mfail ("kiSynth fails on " ++ pshow ty)

-- kind checking
kiCheck :: TEnv -> Type -> Kind -> TCM ()
kiCheck te ty ki = do
  (k1, m1) <- kiSynth te ty
  if klub ki k1 == ki
     then return ()
     else TC.mfail ("Kind " ++ show ki ++ " expected for type " ++ show ty ++ ", but got " ++ show k1)

-- unrestricted strengthening of top entry
strengthenTop :: Exp -> TEnv -> TCM TEnv
strengthenTop e ((_, (Zero, _)) : te) = return te
strengthenTop e ((_, (Many, _)) : te) = return te
strengthenTop e ((x, (One, _)) : te) = TC.mfail ("Linear variable " ++ x ++ " not used in " ++ pshow e)

-- unrestricted strengthening + expanding singletons if needed
strengthen :: Exp -> (Type, TEnv) -> TCM (Type, TEnv)
strengthen e (ty, (x, (mm, tyx)) : te) =
  if mm == One then 
    TC.mfail ("Linear variable " ++ x ++ " not used in " ++ pshow e)
  else
    return (single x tyx ty, te)

teinfo (n, (m, _)) = (n,m)

-- type synthesis and unfolding
tySynthUnfold :: TEnv -> Exp -> TCM (Type, TEnv)
tySynthUnfold te e = do
  (t, te') <- tySynth te e
  tu <- unfold te' t
  return (tu, te')

-- type synthesis
tySynth :: TEnv -> Exp -> TCM (Type, TEnv)
tySynth te e = 
  ap (return (\r@(ty, te) -> D.trace ("Leaving tySynth with " ++ pshow e ++ " : " ++ pshow ty ++ " -| " ++ pshow te) r)) $
  case D.trace ("Invoking tySynth on " ++ pshow te ++ " |- " ++ pshow e) $ e of
  Let x e1 e2 -> do
    (ty1, te1) <- tySynth te e1
    (ki1, mm) <- kiSynth (demoteTE te1) ty1
    (ty2, te2) <- tySynth ((x, (inject mm, ty1)) : te1) e2
    strengthen e (ty2, te2) -- x
  Math m -> tySynthMath te m
  Lit l -> return (tySynthLit l, te)
  Var x -> do
    (mm, tyx) <- maybe (TC.mfail ("No variable " ++ show x)) return $ lookup x te
    mm' <- maybe (TC.mfail ("Illegal use of linear variable " ++ show x)) return $ use mm
    let te' = map upd te
        upd entry@(y, _) = if x == y then (x, (mm', tyx)) else entry
    return (tyx, te')
  Lam mm x tyx e1 -> do
    (kix, mx) <- kiSynth (demoteTE te) tyx
    (ty, te1) <- tySynth ((x, (inject mx, tyx)) : te) e1
    strengthen e (TFun mm x tyx ty, te1)
  Rec f x tyx tyr e1 -> do -- return type is declared
    (kix, mx) <- kiSynth (demoteTE te) tyx
    te1 <- tyCheck ((f, (Many, TFun MMany x tyx tyr)) : (x, (inject mx, tyx)) : te) e1 tyr
    strengthen e (TFun MMany x tyx tyr, te1) >>= strengthen e
  App e1 e2 -> do
    (tf, te1) <- tySynth te e1
    tfu <- unfold te1 tf
    case tfu of
      TFun m x ty1 ty2 -> do
        te2 <- tyCheck te1 e2 ty1
        let tyout = subst x e2 ty2
        _ <- kiSynth (demoteTE te) tyout
        return (tyout, te2)
      _ ->
        TC.mfail ("Function expected, but got " ++ pshow tf ++ " (" ++ pshow tfu ++ ")")

  Pair mul x e1@(Var y) e2 -> do
    let x' = freshvar x $ fv e1 <> Set.delete x (fv e2)
        e2' = subst x (Var x') e2
        ty1' = TSingle y
    (ty1, te1) <- tySynth te e1
    (ki1, mu1) <- kiSynth (demoteTE te) ty1
    (ty2, te2) <- tySynth ((x', (demote mu1, ty1')) : te1) e2'
    (ki2, mu2) <- kiSynth ((x', (demote mu1, ty1')) : demoteTE te) ty2
    strengthen e (TPair mul x' ty1' ty2, te2)

  Pair mul x e1 e2 -> do
    let x' = freshvar x $ fv e1 <> Set.delete x (fv e2)
        e2' = subst x (Var x') e2
    (ty1, te1) <- tySynth te e1
    (ki1, mu1) <- kiSynth (demoteTE te) ty1
    (ty2, te2) <-
      case valueEquiv (demoteTE te) e1 of
        Just (elab, tlabels@(TLab _)) -> do
          (ty2, teq_te2) <-
            tySynth (("*asi*", (demote mu1, TEqn (Var x') elab tlabels)) :
                     (x', (demote mu1, ty1)) :
                     te1) e2'
          return (ty2, tail teq_te2)
        _ ->
          tySynth ((x', (demote mu1, ty1)) : te1) e2'
    (ki2, mu2) <- kiSynth ((x', (demote mu1, ty1)) : demoteTE te) ty2
    strengthen e (TPair mul x' ty1 ty2, te2)
  LetPair x y e1 e2 -> D.trace ("Entering " ++ pshow e) $ do
    (tp, te1) <- tySynth te e1
    tpu <- unfold te1 tp
    case tpu of
      TPair mp z ty1 ty2 -> do
        let ty2' = subst z (Var x) ty2
        (ki1, mu1) <- kiSynth (demoteTE te) ty1
        (ki2, mu2) <- kiSynth ((x, (demote mu1, ty1)) : demoteTE te) ty2'
        (ty3, te2) <- case ty1 of
          TLab labs ->
            tySynth ((y, (inject mu2, ty2')) : (x, (inject mu1, ty1)) : te1) 
                      $ Case (Var x) (map (,e2) labs)
          _ ->
            tySynth ((y, (inject mu2, ty2')) : (x, (inject mu1, ty1)) : te1) e2
        D.trace ("LetPair/kiSynth " ++ pshow (te2, ty3)) $ return ()
        (ki3, mu3) <- kiSynth (demoteTE te2) ty3
        strengthen e (ty3, te2) >>= strengthen e
      _ ->
        TC.mfail ("Pair expected, but got " ++ pshow tp ++ " (" ++ pshow tpu ++ ")")
  Fst e1 -> do                  -- only if second component can be discarded
    (tp, te1) <- tySynth te e1
    tpu <- unfold te1 tp
    case tpu of
      TPair mp z ty1 ty2 -> do
        (ki1, mu1) <- kiSynth (demoteTE te) ty1
        (ki2, mu2) <- kiSynth ((z, (demote mu1, ty1)) : demoteTE te) ty2
        case mu2 of
          MMany -> return (ty1, te1)
          _ -> TC.mfail "Fst: MMany expected"
      _ ->
        TC.mfail ("Fst: pair expected")
  Snd e1 -> do                  -- only if first component can be discarded
    (tp, te1) <- tySynth te e1
    tpu <- unfold te1 tp
    case tpu of
      TPair mp z ty1 ty2 -> do
        (ki1, mu1) <- kiSynth (demoteTE te) ty1
        let ty2' = subst z (Fst e1) ty1
        (ki2, mu2) <- kiSynth (demoteTE te) ty2'
        case mu1 of
          MMany -> return (ty2', te1)
          _ -> TC.mfail "Snd: MMany expected"
      _ ->
        TC.mfail ("Snd: pair expected")
  Fork e1 -> do
    te1 <- tyCheck te e1 TUnit
    return (TUnit, te1)
  New ty -> do
    kiCheck (demoteTE te) ty Kssn
    return (TPair MOne "" ty (dualof ty), te)
  Send e1 -> do
    (ts, te1) <- tySynth te e1
    tsu <- unfold te1 ts
    case tsu of
      TSend x ty1 ty2 ->
        return (TFun MOne x ty1 ty2, te1)
      _ ->
        TC.mfail ("Send expected, but got " ++ pshow ts ++ " (" ++ pshow tsu ++ ")")
  Recv e1 -> do
    (tr, te1) <- tySynth te e1
    tru <- unfold te1 tr
    case tru of
      TRecv x ty1 ty2 ->
        return (TPair MOne x ty1 ty2, te1)
      _ ->
        TC.mfail ("Recv expected, but got " ++ pshow tr ++ " (" ++ pshow tru ++ ")")
  Case e1 cases
    | Just (Lit (LLab lab), TLab labels) <- valueEquiv (demoteTE te) e1 ->
      do elab <- maybe (TC.mfail ("No case for label " ++ show lab)) return $ lookup lab cases
         tySynth te elab
  Case e1@(Var x) cases -> do
    let labels = map fst cases
        tlabels = TLab labels
    _ <- tyCheck (demoteTE te) e1 tlabels
    let flab (lab, elab) = do
          (tylab, teq_telab) <- tySynth (("*lab-e2*", (Many, TEqn e1 (Lit $ LLab lab) tlabels)) : te) elab
          return ((lab, tylab), tail teq_telab)
    ty_te_s <- mapM flab cases
    ty' <- tcaseM te e1 tlabels (map fst ty_te_s)
    -- let ty' = tcase e1 (map fst ty_te_s)
    te' <- tenvJoinN (map snd ty_te_s)
    return (ty', te')
{- -- previous implementation using lub
  Case e1@(Var x) cases -> do
    let labels = map fst cases
        tlabels = TLab labels
    tyCheck (demoteTE te) e1 tlabels
    let flab (lab, elab) = do
          (tylab, telab) <- tySynth (("*lab-e2*", (Many, TEqn e1 (Lab lab) tlabels)) : te) elab
          telab' <- strengthenTop e telab -- drop the equation
          return (TCase e1 ((lab, tylab) : [(lab', TBot) | lab' <- labels, lab' /= lab]),
                  telab')
    ty_te_s <- mapM flab cases
    ty' <- lubn (demoteTE te) (map fst ty_te_s)
    te' <- tenvJoinN (map snd ty_te_s)
    return (ty', te')
-}
  Case _ _ ->
    TC.mfail "illegal case expression"
  NatRec e1 ez n1 tv y tyy es -> do
    _ <- tyCheck (demoteTE te) e1 TNat
    TC.censor (const []) $ TC.mlocal tv (TVar False tv, Kunit) $ do
      (tyz, tez) <- tySynth te ez
      D.traceM ("NatRec: tyz = " ++ pshow tyz)
      (kiy, muy) <- kiSynth (demoteTE te) tyy
      D.traceM ("NatRec: kiy = " ++ pshow (kiy, muy))
      (_, ccz) <- TC.listen (tyCheck te ez tyy)
      let tes_in = (y, (inject muy, tyy)) : (n1, (Many, TNat)) : te
      D.traceM ("NatRec: tes_in = " ++ pshow tes_in)
      (tys, tes) <- tySynth tes_in es
      D.traceM ("NatRec: tys = " ++ pshow tys)
      (_, ccs) <- TC.listen (tyCheck tes_in es tyy)
      D.traceM ("NatRec found constraints " ++ pshow ccz ++ ", " ++ pshow ccs)
      -- hack alert
      let rty = tsubst tv (TNatRec e1 (nonvar tzl tzr) tv (nonvar tsl tsr)) tyy
          tzl :<: tzr = head ccz
          tsl :<: tsr = head ccs
          nonvar (TVar _ _) tr = tr
          nonvar tl _ = tl
      D.traceM ("NatRec returns " ++ pshow rty)
      return (rty, tez)
  _ ->
    TC.mfail ("Unhandled expression: " ++ pshow e)

tySynthLit :: Literal -> Type
tySynthLit = \case
  LInt _ -> TInt
  LNat _ -> TNat
  LLab l -> TLab [l]
  LUnit  -> TUnit

tySynthMath :: TEnv -> MathOp Exp -> TCM (Type, TEnv)
tySynthMath te m = do
  te' <- foldM (\te' e -> tyCheck te' e TInt) te m
  return (TInt, te')

tyCheck :: TEnv -> Exp -> Type -> TCM TEnv
tyCheck te e ty = do
  (tysyn, te') <- tySynth te e
  ki <- subtype (demoteTE te) tysyn ty
  return te'

tenvJoinN :: [TEnv] -> TCM TEnv
tenvJoinN [te] = return te
tenvJoinN (te1:te2:tes) = do
  te12 <- tenvJoin te1 te1
  tenvJoinN (te12:tes)

tenvJoin :: TEnv -> TEnv -> TCM TEnv
tenvJoin = zipWithM tenvEntryJoin

tenvEntryJoin :: TEnvEntry -> TEnvEntry -> TCM TEnvEntry
tenvEntryJoin e1 e2
  | e1 == e2 = return e1
tenvEntryJoin e1 e2 = TC.mfail ("Unmatched environment entries " ++ show e1 ++ " and " ++ show e2)
