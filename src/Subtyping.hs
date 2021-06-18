module Subtyping where

import Control.Applicative
import Data.List (union, intersect)

import qualified Debug.Trace as D

import Syntax
import Kinds

-- value equivalence
valueEquiv' :: TEnv -> Exp -> Maybe (Exp, Type)
valueEquiv' tenv (Lit (LLab name)) = Just (Lit (LLab name), TLab [name])
valueEquiv' tenv (Var name) = 
  let eqnEnv = [(name, (val, ty)) | (_, (Many, TEqn (Var name) val ty)) <- tenv] in
  lookup name eqnEnv
valueEquiv' tenv _ =
  Nothing

valueEquiv tenv exp =
  let r = valueEquiv' tenv exp in
    D.trace ("valueEquiv " ++ show tenv ++ " " ++ show exp ++ " = " ++ show r) r

-- subtyping
subtype' :: TEnv -> Type -> Type -> Maybe Kind
subtype' tenv TUnit TUnit = return Kunit
subtype' tenv TInt TInt = return Kunit
subtype' tenv (TLab ls1) (TLab ls2) | all (`elem` ls2) ls1 = return Kidx
subtype' tenv s@(TFun sm sx sin sout) t@(TFun tm tx tin tout) =
  let nx = "zz" ++ show (length tenv) in
  do kin <- subtype tenv tin sin
     kout <- subtype ((nx, (demote (mult kin), tin)) : tenv)
                     (subst sx (Var nx) sout)
                     (subst tx (Var nx) tout)
     if sm <= tm then return $ kindof tm else fail (show s ++ " <: " ++ show t)
subtype' tenv s@(TPair sm sx sin sout) t@(TPair tm tx tin tout) =
  let nx = "zz" ++ show (length tenv) in
  do kin <- subtype tenv sin tin
     kout <- subtype ((nx, (demote (mult kin), sin)) : tenv)
                     (subst sx (Var nx) sout)
                     (subst tx (Var nx) tout)
     if sm <= tm then return $ kindof tm else fail (show s ++ " <: " ++ show t)
subtype' tenv (TSend sx sin sout) (TSend tx tin tout) =
  let nx = "zz" ++ show (length tenv) in
  do kin <- subtype tenv tin sin
     kout <- subtype ((nx, (demote (mult kin), tin)) : tenv)
                     (subst sx (Var nx) sout)
                     (subst tx (Var nx) tout)
     return Kssn
subtype' tenv (TRecv sx sin sout) (TRecv tx tin tout) =
  let nx = "zz" ++ show (length tenv) in
  do kin <- subtype tenv sin tin
     kout <- subtype ((nx, (demote (mult kin), sin)) : tenv)
                     (subst sx (Var nx) sout)
                     (subst tx (Var nx) tout)
     return Kssn
subtype' tenv (TCase val cases) ty2 
  | Just (Lit (LLab lll), TLab _) <- valueEquiv tenv val = 
  do ty1 <- lookup lll cases
     subtype tenv ty1 ty2
subtype' tenv (TCase val@(Var x) cases) ty2 = 
  do (_, lty@(TLab labs)) <- lookup x tenv
     results <- mapM (\lll -> lookup lll cases >>= \ty1 ->
                              subtype (("*lft*", (Many, TEqn val (Lit $ LLab lll) lty)) : tenv) ty1 ty2)
                     labs
     return $ foldr1 klub results
subtype' tenv ty1 (TCase val cases)
  | Just (Lit (LLab lll), TLab _) <- valueEquiv tenv val =
  do ty2 <- lookup lll cases
     subtype tenv ty1 ty2
subtype' tenv ty1 (TCase val@(Var x) cases) =
  do (_, lty@(TLab labs)) <- lookup x tenv
     results <- mapM (\lll -> lookup lll cases >>= \ty2 ->
                              subtype (("*rgt*", (Many, TEqn val (Lit $ LLab lll) lty)) : tenv) ty1 ty2)
                     labs
     return $ foldr1 klub results
  

-- catchall
subtype' tenv t1 t2 = Nothing

subtype tenv t1 t2 =
  let r = subtype' tenv t1 t2 in
    D.trace ("subtype " ++ show tenv ++ " (" ++ show t1 ++ ") (" ++ show t2 ++ ") = " ++ show r) r

lub', glb' :: TEnv -> Type -> Type -> Maybe Type
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
  | Just (Lit (LLab lll), TLab _) <- valueEquiv tenv val = 
  do ty1 <- lookup lll cases
     lub tenv ty1 ty2
lub' tenv (TCase val@(Var x) cases) ty2 =
  do (_, lty@(TLab labs)) <- lookup x tenv
     results <- mapM (\lll -> lookup lll cases >>= \ty1 ->
                              lub (("*lft*", (Many, TEqn val (Lit $ LLab lll) lty)) : tenv) ty1 ty2)
                     labs
     return $ tcase val (zip labs results)

lub' tenv ty1 (TCase val cases)
  | Just (Lit (LLab lll), TLab _) <- valueEquiv tenv val =
  do ty2 <- lookup lll cases
     lub tenv ty1 ty2
lub' tenv ty1 (TCase val@(Var x) cases) =
  do (_, lty@(TLab labs)) <- D.traceShowId $ lookup x tenv
     results <- mapM (\lll -> lookup lll cases >>= \ty2 ->
                              lub (("*rgt*", (Many, TEqn val (Lit $ LLab lll) lty)) : tenv) ty1 ty2)
                     labs
     return $ tcase val (zip labs results)

-- fallback
lub' tenv TBot t2 = return t2
lub' tenv t1 TBot = return t1

lub' tenv t1 t2 = Nothing

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
  | Just (Lit (LLab lll), TLab _) <- valueEquiv tenv val = 
  do ty1 <- lookup lll cases
     glb tenv ty1 ty2
glb' tenv (TCase val@(Var x) cases) ty2 =
  do (_, lty@(TLab labs)) <- lookup x tenv
     results <- mapM (\lll -> lookup lll cases >>= \ty1 ->
                              glb (("*lft*", (Many, TEqn val (Lit $ LLab lll) lty)) : tenv) ty1 ty2)
                     labs
     return $ tcase val (zip labs results)

glb' tenv ty1 (TCase val cases)
  | Just (Lit (LLab lll), TLab _) <- valueEquiv tenv val =
  do ty2 <- lookup lll cases
     glb tenv ty1 ty2
glb' tenv ty1 (TCase val@(Var x) cases) =
  do (_, lty@(TLab labs)) <- lookup x tenv
     results <- mapM (\lll -> lookup lll cases >>= \ty2 ->
                              glb (("*rgt*", (Many, TEqn val (Lit $ LLab lll) lty)) : tenv) ty1 ty2)
                     labs
     return $ tcase val (zip labs results)

glb' tenv t1 t2 = Nothing

lub tenv t1 t2 =
  let r = lub' tenv t1 t2 in
    D.trace ("lub " ++ show tenv ++ " (" ++ show t1 ++ ") (" ++ show t2 ++ ") = " ++ show r) r

glb tenv t1 t2 =
  let r = glb' tenv t1 t2 in
    D.trace ("glb " ++ show tenv ++ " (" ++ show t1 ++ ") (" ++ show t2 ++ ") = " ++ show r) r

-- smart constructor that drops the case if all branches are equal (eta reduction)
tcase :: Exp -> [(String, Type)] -> Type
tcase e ((_, t) : cases) | all ((==t).snd) cases = t
tcase e sts = TCase e sts

-- n-ary lub for non-empty lists
lubn :: TEnv -> [Type] -> Maybe Type
lubn te [ty] = return ty
lubn te (ty1:ty2:tys) = do
  ty12 <- lub te ty1 ty2
  lubn te (ty12:tys)
