{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module TCTyping where

import Control.Monad (foldM, zipWithM, ap, when)
import qualified Data.Set as Set

import qualified Config as D

import Syntax
import PrettySyntax
import Kinds
import TCSubtyping
import qualified TCXMonad as TC
import qualified TCSubtyping as TC

-- kind synthesis
kiSynth :: TEnv -> Type -> TCM (Kind, Multiplicity)
kiSynth te (TName b tn) = do
  kentry <- TC.kindLookup tn
  let k = keKind kentry
  return (k, mult k)
kiSynth te TUnit = return (Kunit, MMany)
kiSynth te TInt  = return (Kun, MMany)
kiSynth te TDouble = return (Kun, MMany)
kiSynth te TString = return (Kun, MMany)
kiSynth te TNat  = return (Kun, MMany)
kiSynth te TBot  = return (Kunit, MMany) -- this kind is compatible with everything
kiSynth te TDyn  = return (Kidx, MMany)
kiSynth te (TLab labs) = do
  case labs of
    (_ : _) -> return (Kidx, MMany)
    _ -> TC.mfail "Empty label type"
kiSynth te (TFun m x ty1 ty2) = do
  (k1, m1) <- kiSynth te ty1
  (k2, m2) <- kiSynth ((x, (demote m1, ty1)) : te) ty2
  return (kindof m, m)
kiSynth te ty@(TPair x ty1 ty2) = do
  (k1, m1) <- kiSynth te ty1
  (k2, m2) <- kiSynth ((x, (demote m1, ty1)) : te) ty2
  let mout = max m1 m2
  return (kindof mout, mout)
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
  kentry <- TC.kindLookup v
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

-- unrestricted strengthening of top entry + expanding singleton in return type if needed
strengthen :: Exp -> (Type, TEnv) -> TCM (Type, TEnv)
strengthen e (ty, (x, (mm, tyx)) : te) =
  if mm == One then
    TC.mfail ("Linear variable " ++ x ++ " not used in " ++ pshow e)
  else
    return (single x tyx ty, te)

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
    (ki1, mm)  <- kiSynth (demoteTE te1) ty1
    (ty2, te2) <- tySynth ((x, (inject mm, ty1)) : te1) e2
    strengthen e (ty2, te2) -- x
  Math m ->
    tySynthMath te m
  Lit l ->
    return (tySynthLit l, te)
  Var x -> do
    (mm, tyx) <- maybe (TC.mfail ("No variable " ++ show x)) return $ lookup x te
    mm' <- maybe (TC.mfail ("Illegal use of linear variable " ++ show x)) return $ use mm
    let te'
          | mm' == mm = te
          | otherwise = upd te -- mapping over te is wrong because there may be more than one entry for x...
        upd [] = []
        upd (entry@(y, _) : rest)
          | x == y    = (x, (mm', tyx)) : rest
          | otherwise = entry : upd rest
    return (tyx, te')
  Lam mm x tyx e1 -> do
    (kix, mx) <- kiSynth (demoteTE te) tyx
    tyxBound <- tyBound (demoteTE te) tyx
    let foundCaseInTypeBeforeCaseInTerm = findCaseOnVar x e1
        tebody = (x, (inject mx, tyx)) : te
    D.traceOnlyM "tysynth" ("implicit case for " ++ pshow e ++ ": " ++ pshow  (tyxBound, foundCaseInTypeBeforeCaseInTerm))
    case (tyxBound, foundCaseInTypeBeforeCaseInTerm) of
      (TLab labs, True) -> do -- insert an implicit case
        ty_te_s <- mapM (\lab -> tySynth (("*lam*", (Many, TEqn (Var x) (Lit $ LLab lab) tyxBound)) : tebody) e1 >>= strengthen e1) labs
        ty' <- tcaseM (demoteTE tebody) (Var x) tyxBound (zip labs $ map fst ty_te_s)
        te' <- tenvJoinN (map snd ty_te_s)
        strengthen e (TFun mm x tyx ty', te')
      _ -> do
        (ty, te1) <- tySynth tebody e1
        strengthen e (TFun mm x tyx ty, te1)
  App e1 e2 -> do
    (tf, te1) <- tySynth te e1
    tfu <- unfold te1 tf
    case tfu of
      TFun m x ty1 ty2 -> do
        te2 <- tyCheck te1 e2 ty1
        let tyout = subst x e2 ty2
        _ <- kiSynth (demoteTE te) tyout
        return (tyout, te2)
      TDyn -> do
        TC.failUnlessGradual ("Function expected, but got " ++ pshow tf ++ " (" ++ pshow tfu ++ ")")
        te2 <- tyCheck te1 e2 TDyn
        return (TDyn, te2)
      _ ->
        TC.mfail ("Function expected, but got " ++ pshow tf ++ " (" ++ pshow tfu ++ ")")

  Pair _ x e1@(Var y) e2 -> do
    let x' = freshvar x $ fv e1 <> Set.delete x (fv e2)
        e2' = subst x (Var x') e2
        ty1' = TSingle y
    (ty1, te1) <- tySynth te e1
    (ki1, mu1) <- kiSynth (demoteTE te) ty1
    (ty2, te2) <- tySynth ((x', (demote mu1, ty1')) : te1) e2'
    (ki2, mu2) <- kiSynth ((x', (demote mu1, ty1')) : demoteTE te) ty2
    strengthen e (TPair x' ty1' ty2, te2)

  Pair _ x e1 e2 -> do
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
    strengthen e (TPair x' ty1 ty2, te2)
  LetPair x y e1 e2 -> D.trace ("Entering " ++ pshow e) $ do
    (tp, te1) <- tySynth te e1
    tpu <- unfold te1 tp
    case tpu of
      TPair z ty1 ty2 -> do
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
      TDyn -> do                -- matching happens here!
        TC.failUnlessGradual ("Pair expected, but got " ++ pshow tp ++ " (" ++ pshow tpu ++ ")")
        (ty3, te2) <- tySynth ((y, (Many, TDyn)) : (x, (Many, TDyn)) : te1) e2
        (ki3, mu3) <- kiSynth (demoteTE te2) ty3
        let ty3_without_xy = subst x (Fst e1) (subst y (Snd e1) ty3) -- FISHY
        strengthen e (ty3_without_xy, te2) >>= strengthen e
      _ ->
        TC.mfail ("Pair expected, but got " ++ pshow tp ++ " (" ++ pshow tpu ++ ")")
  Fst e1 -> do                  -- only if second component can be discarded
    (tp, te1) <- tySynth te e1
    tpu <- unfold te1 tp
    case tpu of
      TPair z ty1 ty2 -> do
        (ki1, mu1) <- kiSynth (demoteTE te) ty1
        (ki2, mu2) <- kiSynth ((z, (demote mu1, ty1)) : demoteTE te) ty2
        case mu2 of
          MMany -> return (ty1, te1)
          _ -> TC.mfail "Fst: MMany expected"
      TDyn -> do
        TC.failUnlessGradual ("Fst: pair expected")
        return (TDyn, te1)
      _ ->
        TC.mfail ("Fst: pair expected")
  Snd e1 -> do                  -- only if first component can be discarded
    (tp, te1) <- tySynth te e1
    tpu <- unfold te1 tp
    case tpu of
      TPair z ty1 ty2 -> do
        (ki1, mu1) <- kiSynth (demoteTE te) ty1
        let ty2' = subst z (Fst e1) ty1
        (ki2, mu2) <- kiSynth (demoteTE te) ty2'
        case mu1 of
          MMany -> return (ty2', te1)
          _ -> TC.mfail "Snd: MMany expected"
      TDyn -> do
        TC.failUnlessGradual ("Snd: pair expected")
        return (TDyn, te1)
      _ ->
        TC.mfail ("Snd: pair expected")
  Fork e1 -> do
    te1 <- tyCheck te e1 TUnit
    return (TUnit, te1)
  New ty -> do
    kiCheck (demoteTE te) ty Kssn
    return (TPair "" ty (dualof ty), te)
  -- I've got no real clue of what I am doing here hope it kind of works
  Connect e0 ty e1 e2 -> do
    kiCheck (demoteTE te) ty Kssn
    return (ty, te)
  Accept e1 ty -> do
    kiCheck (demoteTE te) ty Kssn
    return (ty, te)
  Send e1 -> do
    (ts, te1) <- tySynth te e1
    tsu <- unfold te1 ts
    case tsu of
      TSend x ty1 ty2 ->
        return (TFun MOne x ty1 ty2, te1)
      TDyn -> do
        TC.failUnlessGradual ("Send expected, but got " ++ pshow ts ++ " (" ++ pshow tsu ++ ")")
        return (TFun MOne "d" TDyn TDyn, te1)
      _ ->
        TC.mfail ("Send expected, but got " ++ pshow ts ++ " (" ++ pshow tsu ++ ")")
  Recv e1 -> do
    (tr, te1) <- tySynth te e1
    tru <- unfold te1 tr
    case tru of
      TRecv x ty1 ty2 ->
        return (TPair x ty1 ty2, te1)
      TDyn -> do
        TC.failUnlessGradual ("Recv expected, but got " ++ pshow tr ++ " (" ++ pshow tru ++ ")")
        return (TPair "d" TDyn TDyn, te1)
      _ ->
        TC.mfail ("Recv expected, but got " ++ pshow tr ++ " (" ++ pshow tru ++ ")")

  Case e1 cases -> do
    D.traceOnlyM "tysynth" (pshow e)
    (t1, te1) <- tySynth te e1
    tbound <- tyBound (demoteTE te) t1
    when (tbound == TDyn) $
      TC.failUnlessGradual "case on value with dynamic type bound not allowed - implementation restriction"
    mEquiv <- valueEquivM (demoteTE te) e1
    case mEquiv of
      Just (Lit (LLab lab), TLab labels) -> do
        elab <- maybe (TC.mfail ("No case for label " ++ show lab)) return $ lookup lab cases
        tySynth te elab
      Nothing -> do
        D.traceOnlyM "tysynth" ("case header has type " ++ pshow t1)
        let labels = map fst cases
            tlabels = TLab labels
            checked = case tbound of
              TLab labs -> labs
              _ -> labels
            tChecked = TLab checked
        _ <- subtype (demoteTE te) t1 tlabels
        mkEqn <- case e1 of
              Var x ->                 return $ \lab -> TEqn (Var x) (Lit $ LLab lab) tChecked
              (Cast (Var x) t1 _t2) -> return $ \lab -> TEqn (Var x) (Cast (Lit (LLab lab)) tChecked t1) t1
              _ -> TC.mfail ("case header must be variable or cast, but got " ++ pshow e1)
        let flab (lab, elab) = do
              (tylab, teq_telab) <- tySynth (("*lab-e2*", (Many, mkEqn lab)) : te) elab
              return ((lab, tylab), tail teq_telab)
        ty_te_s <- mapM flab (filter ((`elem` checked) . fst) cases)
        ty' <- tcaseM te e1 tChecked (map fst ty_te_s)
        -- let ty' = tcase e1 (map fst ty_te_s)
        te' <- tenvJoinN (map snd ty_te_s)
        return (ty', te')

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
  Cast e t1 t2 -> tySynthCast te e t1 t2
  NewNatRec f n tv tyy ez n1 es -> do
    -- _ <- tyCheck (demoteTE te) e1 TNat
    TC.censor (const []) $ TC.mlocal tv (TVar False tv, Kunit) $ do
      (tyz, tez) <- tySynth te ez
      D.traceM ("NewNatRec: tyz = " ++ pshow tyz)
      (kiy, muy) <- kiSynth (demoteTE te) tyy
      D.traceM ("NewNatRec: kiy = " ++ pshow (kiy, muy))
      (_, ccz) <- TC.listen (tyCheck te ez tyy)
      let tes_in = (f, (Many, TFun MMany n TNat tyy)) : (n1, (Many, TNat)) : te
      D.traceM ("NewNatRec: tes_in = " ++ pshow tes_in)
      (tys, tes) <- tySynth tes_in es
      D.traceM ("NewNatRec: tys = " ++ pshow tys)
      (_, ccs) <- TC.listen (tyCheck tes_in es tyy)
      D.traceM ("NewNatRec found constraints " ++ pshow ccz ++ ", " ++ pshow ccs)
      -- hack alert
      let rty = tsubst tv (TNatRec (Var n) (nonvar tzl tzr) tv (nonvar tsl tsr)) tyy
          tzl :<: tzr = head ccz
          tsl :<: tsr = head ccs
          nonvar (TVar _ _) tr = tr
          nonvar tl _ = tl
      D.traceM ("NewNatRec returns body " ++ pshow rty)
      return (TFun MMany n TNat rty, tez)
  Rec f x e1 e0 -> return (TDyn, te)
  _ -> TC.mfail ("Unhandled expression: " ++ pshow e)

tySynthCast :: TEnv -> Exp -> Type -> Type -> TCM (Type, TEnv)
tySynthCast te e t1 t2 = (,) t2 <$> tyCheck te e t1

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

-- findCaseOnVar x e
-- does e contain a case on x in a type, before it cases on x not in the expression?
findCaseOnVar :: Ident -> Exp -> Bool
findCaseOnVar x = \case
  Let y e1 e2 ->
    findCaseOnVar x e1 || x /= y && findCaseOnVar x e2
  Math mo ->
    foldr (\e b -> findCaseOnVar x e || b) False mo
  Lit _ ->
    False
  Succ e1 ->
    findCaseOnVar x e1
  NatRec e0 e1 y ti z tz e2 ->
    findTCaseOnVar x tz ||
    findCaseOnVar x e0 || findCaseOnVar x e1 || x /= y && x /= z && findCaseOnVar x e2
  NewNatRec f y ti ty e1 z e2 ->
    findTCaseOnVar x ty ||
    x /= f && x /= y && (findCaseOnVar x e1 || x /= z && findCaseOnVar x e2)
  Var _ ->
    False
  Lam _ y ty e1 ->
    findTCaseOnVar x ty ||
    x /= y && findCaseOnVar x e1
  Rec f y e1 e2 ->
    x /= f && x /= y && (findCaseOnVar x e1 || findCaseOnVar x e2)
  App e1 e2 ->
    findCaseOnVar x e1 || findCaseOnVar x e2
  Pair _ y e1 e2 ->
    findCaseOnVar x e1 || x /= y && findCaseOnVar x e2
  LetPair y z e1 e2 ->
    findCaseOnVar x e1 || x /= y && x /= z && findCaseOnVar x e2
  Fst e1 ->
    findCaseOnVar x e1
  Snd e1 ->
    findCaseOnVar x e1
  Fork e1 ->
    findCaseOnVar x e1
  New t ->
    findTCaseOnVar x t
  Send e1 ->
    findCaseOnVar x e1
  Recv e1 ->
    findCaseOnVar x e1
  Case e1 cases ->
    case e1 of
      Var y | x == y -> False
      _ -> foldr (||) (findCaseOnVar x e1) (map (findCaseOnVar x . snd) cases)
  Cast e1 t1 t2 ->
    findTCaseOnVar x t1 || findTCaseOnVar x t2 || findCaseOnVar x e1

findTCaseOnVar :: Ident -> Type -> Bool
findTCaseOnVar x = \case
  TUnit -> False
  TInt -> False
  TDouble -> False
  TBot -> False
  TDyn -> False
  TNat -> False
  TString -> False
  TNatLeq _ -> False
  TNatRec val ty ti tyz ->
    case val of
      Var y | x == y -> True
      _ -> findTCaseOnVar x ty || findTCaseOnVar x tyz
  TVar _ _ -> False
  TAbs _ _ _ -> False
  TName _ _ -> False
  TLab _ -> False
  TFun _ y ty1 ty2 ->
    findTCaseOnVar x ty1 || x /= y && findTCaseOnVar x ty2
  TPair y ty1 ty2 ->
    findTCaseOnVar x ty1 || x /= y && findTCaseOnVar x ty2
  TSend y ty1 ty2 ->
    findTCaseOnVar x ty1 || x /= y && findTCaseOnVar x ty2
  TRecv y ty1 ty2 ->
    findTCaseOnVar x ty1 || x /= y && findTCaseOnVar x ty2
  TCase val tcases ->
    case val of
      Var y | x == y -> True
      _ -> foldr (||) (findCaseOnVar x val) (map (findTCaseOnVar x . snd) tcases)
  TEqn _ _ _ -> False
  TSingle _ -> False
