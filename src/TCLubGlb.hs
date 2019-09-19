module TCLubGlb where

{-
-- TODO: lift to generate mu-types
lub', glb' :: TEnv -> Type -> Type -> TCM Type
lub' tenv TUnit TUnit = return TUnit
lub' tenv TInt TInt = return TInt
lub' tenv (TLab labs1) (TLab labs2) = return $ TLab (labs1 `union` labs2)
lub' tenv (TFun tm tx t1 t2) (TFun sm sx s1 s2) =
  let rx = "zz" ++ show (length tenv) 
  in do
    r1 <- glb tenv t1 s1
    r2 <- lub ((rx, (Many, r1)) : tenv) (subst tx (Var rx) t2) (subst sx (Var rx) s2)
    return $ TFun (max tm sm) rx r1 r2 

lub' tenv (TSend tx t1 t2) (TSend sx s1 s2) =
  let rx = "zz" ++ show (length tenv) 
  in do
    r1 <- glb tenv t1 s1
    r2 <- lub ((rx, (Many, r1)) : tenv) (subst tx (Var rx) t2) (subst sx (Var rx) s2)
    return $ TSend rx r1 r2 

lub' tenv (TPair tm tx t1 t2) (TPair sm sx s1 s2) =
  let rx = "zz" ++ show (length tenv) 
  in do
    r1 <- lub tenv t1 s1
    r2 <- lub ((rx, (Many, r1)) : tenv) (subst tx (Var rx) t2) (subst sx (Var rx) s2)
    return $ TPair (max tm sm) rx r1 r2 

lub' tenv (TRecv tx t1 t2) (TRecv sx s1 s2) =
  let rx = "zz" ++ show (length tenv) 
  in do
    r1 <- lub tenv t1 s1
    r2 <- lub ((rx, (Many, r1)) : tenv) (subst tx (Var rx) t2) (subst sx (Var rx) s2)
    return $ TRecv rx r1 r2 

lub' tenv (TCase val cases) ty2
  | Just (Lab lll, TLab _) <- valueEquiv tenv val = 
  do ty1 <- lablookup lll cases
     lub tenv ty1 ty2
lub' tenv (TCase val@(Var x) cases) ty2 =
  do (lty, labs) <- varlookupLabel x tenv
     results <- mapM (\lll -> lablookup lll cases >>= \ty1 ->
                              lub (("*lft*", (Many, TEqn val (Lab lll) lty)) : tenv) ty1 ty2)
                     labs
     return $ tcase val lty (zip labs results)

lub' tenv ty1 (TCase val cases)
  | Just (Lab lll, TLab _) <- valueEquiv tenv val =
  do ty2 <- lablookup lll cases
     lub tenv ty1 ty2
lub' tenv ty1 (TCase val@(Var x) cases) =
  do (lty, labs) <- varlookupLabel x tenv
     results <- mapM (\lll -> lablookup lll cases >>= \ty2 ->
                              lub (("*rgt*", (Many, TEqn val (Lab lll) lty)) : tenv) ty1 ty2)
                     labs
     return $ tcase val lty (zip labs results)

-- fallback
lub' tenv TBot t2 = return t2
lub' tenv t1 TBot = return t1

lub' tenv t1 t2 = TC.mfail ("LUB fails for " ++ pshow t1 ++ " and " ++ pshow t2)

----------

glb' tenv TUnit TUnit = return TUnit
glb' tenv TInt TInt = return TInt
glb' tenv (TLab labs1) (TLab labs2) = return $ TLab (labs1 `intersect` labs2)
glb' tenv (TFun tm tx t1 t2) (TFun sm sx s1 s2) =
  let rx = "zz" ++ show (length tenv) 
  in do
    r1 <- lub tenv t1 s1
    r2 <- glb ((rx, (Many, r1)) : tenv) (subst tx (Var rx) t2) (subst sx (Var rx) s2)
    return $ TFun (min tm sm) rx r1 r2 

glb' tenv (TSend tx t1 t2) (TSend sx s1 s2) =
  let rx = "zz" ++ show (length tenv) 
  in do
    r1 <- lub tenv t1 s1
    r2 <- glb ((rx, (Many, r1)) : tenv) (subst tx (Var rx) t2) (subst sx (Var rx) s2)
    return $ TSend rx r1 r2 

glb' tenv (TPair tm tx t1 t2) (TPair sm sx s1 s2) =
  let rx = "zz" ++ show (length tenv) 
  in do
    r1 <- glb tenv t1 s1
    r2 <- glb ((rx, (Many, r1)) : tenv) (subst tx (Var rx) t2) (subst sx (Var rx) s2)
    return $ TPair (min tm sm) rx r1 r2 

glb' tenv (TRecv tx t1 t2) (TRecv sx s1 s2) =
  let rx = "zz" ++ show (length tenv) 
  in do
    r1 <- glb tenv t1 s1
    r2 <- glb ((rx, (Many, r1)) : tenv) (subst tx (Var rx) t2) (subst sx (Var rx) s2)
    return $ TRecv rx r1 r2 
    
glb' tenv (TCase val cases) ty2
  | Just (Lab lll, TLab _) <- valueEquiv tenv val = 
  do ty1 <- lablookup lll cases
     glb tenv ty1 ty2
glb' tenv (TCase val@(Var x) cases) ty2 =
  do (lty, labs) <- varlookupLabel x tenv
     results <- mapM (\lll -> lablookup lll cases >>= \ty1 ->
                              glb (("*lft*", (Many, TEqn val (Lab lll) lty)) : tenv) ty1 ty2)
                     labs
     return $ tcase val lty  (zip labs results)

glb' tenv ty1 (TCase val cases)
  | Just (Lab lll, TLab _) <- valueEquiv tenv val =
  do ty2 <- lablookup lll cases
     glb tenv ty1 ty2
glb' tenv ty1 (TCase val@(Var x) cases) =
  do (lty, labs) <- varlookupLabel x tenv
     results <- mapM (\lll -> lablookup lll cases >>= \ty2 ->
                              glb (("*rgt*", (Many, TEqn val (Lab lll) lty)) : tenv) ty1 ty2)
                     labs
     return $ tcase val lty (zip labs results)

glb' tenv t1 t2 = TC.mfail ("GLB fails for " ++ pshow t1 ++ " and " ++ pshow t2)

lub tenv t1 t2 = do
  r <- lub' tenv t1 t2
  return $ D.trace ("lub " ++ pshow tenv ++ " (" ++ pshow t1 ++ ") (" ++ pshow t2 ++ ") = " ++ pshow r) r

glb tenv t1 t2 = do
  r <- glb' tenv t1 t2
  return $ D.trace ("glb " ++ pshow tenv ++ " (" ++ pshow t1 ++ ") (" ++ pshow t2 ++ ") = " ++ pshow r) r
-}

{-
-- n-ary lub for non-empty lists
lubn :: TEnv -> [Type] -> TCM Type
lubn te [ty] = return ty
lubn te (ty1:ty2:tys) = do
  ty12 <- lub te ty1 ty2
  lubn te (ty12:tys)
-}
