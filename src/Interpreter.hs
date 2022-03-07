{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Interpreter
  ( interpret
  , evalDFun
  , createPEnv
  ) where

import qualified Config as C
import Syntax
import PrettySyntax
import qualified Control.Concurrent.Chan as Chan
import Control.Concurrent (forkIO)
import Control.Exception (throw)
import Data.Foldable (find)
import Data.Maybe (fromJust)
import ProcessEnvironment
import qualified Control.Monad as M
import Control.Monad.Reader as R
import Control.Applicative ((<|>))

blame :: Exp -> a
blame exp = throw $ CastException exp

-- | interpret the "main" value in an ldgv file given over stdin
interpret :: [Decl] -> IO Value
interpret decls = do
  -- gather Type and Function definitions
  let penv = createPEnv $ filter isInterestingDecl decls
      isInterestingDecl DFun {} = True
      isInterestingDecl DType {} = True
      isInterestingDecl _ = False
  -- find the main DFun
  case lookup "main" penv of
    Just (VDecl decl) -> R.runReaderT (evalDFun decl) (penv, [])
    _ -> throw $ LookupException "main"

-- | interpret a DFun (Function declaration)
evalDFun :: Decl -> InterpretM Value
evalDFun decl@(DFun name [] e _) = interpret' e  -- a Declaration without free variables can be just interpreted
evalDFun decl@(DFun name binds e _) =
  let lambda = foldr (\(mul, id, ty) -> Lam mul id ty) e binds
  in interpret' lambda
evalDFun decl = fail ("evalDFun expects DFun, got: " ++ show decl)

-- | interpret a single Expression
interpret' :: Exp ->  InterpretM Value
interpret' e = ask >>= \penv ->
  M.ap
  (return $ \val -> C.trace ("Leaving interpretation of " ++ pshow e ++ " with value " ++ show val) val) $
  (C.trace ("Invoking interpretation on " ++ pshow e) . eval) e

eval :: Exp -> InterpretM Value
eval = \case
  Succ e -> interpretMath $ Add (Lit (LInt 1)) e
  Rec f x e1 e0 -> ask >>= \(env, _) -> return $ VRec env f x e1 e0
  NatRec e1 e2 i1 t1 i2 t e3 -> do
  -- returns a function indexed over e1 (should be a variable pointing to a Nat)
  -- e1 should be the recursive variable which gets decreased each time the
  -- non-zero case is evaluated
  -- e2 is the zero case
  -- e3 is the nonzero case
    i <- interpret' e1
    case i of
      VInt 0 -> interpret' e2
      VInt 1 -> do
        zero <- interpret' e2
        R.local (extendEnv i1 (VInt 0) . extendEnv i2 zero) (interpret' e3)
      VInt n -> do
        -- interpret the n-1 case i2 and add it to the env
        -- together with n before interpreting the body e3
        let lowerEnv = extendEnv i1 (VInt $ n-1)
        lower <- R.local lowerEnv (interpret' $ NatRec (Var i1) e2 i1 t1 i2 t e3)
        R.local (extendEnv i2 lower . lowerEnv) (interpret' e3)
      _ -> throw $ RecursorException "Evaluation of 'natrec x...' must yield Nat value"
  --NewNatRec
  Lam _ i _ e -> do
    (env, _) <- ask
    return $ VFunc env i e
  cast@(Cast e t1 t2) -> do
    C.traceIO $ "Interpreting cast expression: " ++ pshow cast
    v <- interpret' e
    C.traceIO ("Evaluating expression " ++ pshow e ++ " to value " ++ show v)
    nft1 <- evalType t1
    C.traceIO $ "Evaluating type " ++ show t1 ++ " to normal form " ++ show nft1
    nft2 <- evalType t2
    C.traceIO $ "Evaluating type " ++ show t2 ++ " to normal form " ++ show nft2
    case v of
      VPair {} -> do
        C.traceIO $ "Interpreting pair cast expression: Value(" ++ show v ++ ") NFType(" ++ show nft1 ++ ") NFType(" ++ show nft2 ++ ")"
        v' <- lift $ reducePairCast v (toNFPair nft1) (toNFPair nft2)
        maybe (blame cast) return v'
      _ -> let v' = reduceCast v nft1 nft2 in maybe (blame cast) return (C.trace ("Cast reduction of " ++ show v ++ " : " ++ show nft1 ++ " => " ++ show nft2 ++ " results in value " ++ show v') v')
  Var s -> do
    (penv, _) <- ask
    maybe (throw $ LookupException s) (liftIO . pure) (lookup s penv)
  Let s e1 e2 -> interpret' e1 >>= \v -> R.local (extendEnv s v) (interpret' e2)
  Math m -> interpretMath m
  Lit l -> return (interpretLit l)
  e@(App e1 e2) -> do
    liftIO $ C.traceIO $ "Arguments for (" ++ pshow e1 ++ ") are ("  ++ pshow e2 ++ ")"
    val <- interpret' e1
    arg <- interpret' e2
    interpretApp e val arg
  Pair mul s e1 e2 -> do
    v1 <- interpret' e1
    v2 <- R.local (extendEnv s v1) (interpret' e2)
    return $ VPair v1 v2
  LetPair s1 s2 e1 e2 -> do
    interpret' e1 >>= \(VPair v1 v2) -> R.local (extendEnv s2 v2 . extendEnv s1 v1) (interpret' e2)
  fst@(Fst e) -> interpret' e >>= \(VPair s1 s2) -> return s1
  snd@(Snd e) -> interpret' e >>= \(VPair s1 s2) -> return s2
  Fork e -> do
    penv <- ask
    liftIO $ forkIO (do
      res <- R.runReaderT (interpret' e) penv
      C.traceIO "Ran a forked operation")
    return VUnit
  New t -> do
    r <- liftIO Chan.newChan
    w <- liftIO Chan.newChan
    return $ VPair (VChan r w) (VChan w r)
  Send e -> VSend <$> interpret' e
  Recv e -> do
    interpret' e >>= \v@(VChan c _) -> do
      val <- liftIO $ Chan.readChan c
      liftIO $ C.traceIO $ "Read " ++ show val ++ " from Chan "
      return $ VPair val v
  Case e cases -> do
    interpret' e >>= \(VLabel s) ->
      interpret' $ fromJust $ lookup s cases
  e -> throw $ NotImplementedException e

-- Exp is only used for blame
interpretApp :: Exp -> Value -> Value -> InterpretM Value
interpretApp e (VDecl d) w = evalDFun d >>= \vfunc -> interpretApp e vfunc w
interpretApp _ (VFunc env s exp) w = R.local (\(_, aenv) -> extendEnv s w (env,aenv)) (interpret' exp)
interpretApp e (VFuncCast v (FuncType penv aenv s t1 t2) (FuncType penv' aenv' s' t1' t2')) w' = do
  env0 <- ask
  let
    interpretAppCast :: IO Value
    interpretAppCast = do
      C.traceIO ("Attempting function cast in application (" ++ show v ++ ") with (" ++ show w' ++ ")")
      nft1  <- R.runReaderT (evalType t1)  (penv, aenv)
      nft1' <- R.runReaderT (evalType t1') (penv', aenv')
      w <- maybe (blame e) return (reduceCast w' nft1' nft1)
      nft2' <- R.runReaderT (evalType t2') (extendEnv s' w' (penv', aenv'))
      nft2  <- R.runReaderT (evalType t2)  (extendEnv s  w  (penv, aenv))
      u  <- R.runReaderT (interpretApp e v w) env0
      u' <- maybe (blame e) return (reduceCast u nft2 nft2')
      C.traceIO ("Function cast in application results in: " ++ show u')
      return u'
  lift interpretAppCast
interpretApp _ rec@(VRec env f x e1 e0) (VInt n)
  | n  < 0 = throw RecursorNotNatException
  | n == 0 = interpret' e0
  | n  > 0 = let env' = extendEnv f rec (extendEnv x (VInt (n-1)) (env, []))
             in R.local (const env') (interpret' e1)
interpretApp _ (VSend v@(VChan _ c)) w = liftIO (Chan.writeChan c w) >> return v
interpretApp e _ _ = throw $ ApplicationException e

interpretLit :: Literal -> Value
interpretLit = \case
  LInt i -> VInt i
  LNat n -> VInt n
  LLab l -> VLabel l
  LDouble d -> VDouble d
  LString s -> VString s
  LUnit  -> VUnit

interpretMathOp :: Exp -> Exp -> (Int -> Int -> Int) -> (Double -> Double -> Double) -> InterpretM Value
interpretMathOp a b opInt opDouble = do
  v <- interpret' a
  w <- interpret' b
  return $ case (v, w) of
    (VInt x, VInt y) -> VInt (opInt x y)
    (VDouble x, VDouble y) -> VDouble (opDouble x y)
    (_, _) -> throw $ MathException (show v ++ " -> " ++ show w ++ " -> a: did not yield a value")

interpretMath :: MathOp Exp -> InterpretM Value
interpretMath = \case
  Add a b -> interpretMathOp a b (+) (+)
  Sub a b -> interpretMathOp a b (-) (-)
  Mul a b -> interpretMathOp a b (*) (*)
  Div a b -> interpretMathOp a b quot (/)
  Neg a   -> interpret' a >>= (\v -> return $ case v of
    VInt x -> VInt (negate x)
    VDouble x -> VDouble (negate x)
    _ -> throw $ MathException ("negate " ++ show v ++ ": did not yield a value"))

evalType :: Type -> InterpretM NFType
evalType = \case
  TUnit -> return $ NFGType GUnit
  TInt -> return NFInt
  TDouble -> return NFDouble
  TString -> return NFString
  TBot -> return NFBot
  TDyn -> return NFDyn
  TNat -> return $ NFGType GNat
  TNatLeq n -> return $ NFGType $ GNatLeq n
  TNatRec e t1 tid t2 -> do
    v <- interpret' e
    case v of
      VInt 0 -> evalType t2
      VInt n -> if n < 0
        then throw RecursorNotNatException
        else R.local
          (\(penv, aenv) ->
            (penv, (tid, NREntry penv (VInt $ n-1) tid t1 t2) : aenv)
          ) (evalType t2)
      _ -> throw $ RecursorException "Evaluation of 'natrec x...' must yield Nat value"
  TName _ s -> do
    (penv, aenv) <- ask
    case lookup s aenv of
      Just (NREntry _ (VInt n) tid t1 t2) -> evalType (TNatRec (Lit $ LNat n) t1 tid t2)
      Just _ -> throw RecursorNotNatException
      Nothing -> case lookup s penv of
        Just (VType t)  -> evalType t
        _ -> throw $ LookupException s
  TLab ls -> return $ NFGType $ GLabel $ labelsFromList ls
  TFun  _ s TDyn TDyn -> return $ NFGType $ GFunc s
  TFun  _ s t1 t2 -> ask >>= \(penv, aenv) -> return $ NFFunc $ FuncType penv aenv s t1 t2
  TPair _ s TDyn TDyn -> return $ NFGType $ GPair s
  TPair _ s t1 t2 -> ask >>= \(penv, aenv) -> return $ NFPair $ FuncType penv aenv s t1 t2
  TCase exp labels -> interpret' exp >>= \(VLabel l) ->
    let entry = find (\(l', _) -> l == l') labels
    in maybe (return NFBot) (evalType . snd) entry
  t -> throw $ TypeNotImplementedException t

reduceCast :: Value -> NFType -> NFType -> Maybe Value
reduceCast v t1 t2 = castIsValue v t1 t2 <|> reduceCast' v t1 t2

-- Cast-Is-Value: return correct value if arguments already form a value
castIsValue :: Value -> NFType -> NFType -> Maybe Value
castIsValue v (NFGType gt) NFDyn = Just $ VDynCast v gt
castIsValue v (NFFunc ft1)        (NFFunc ft2)        = Just $ VFuncCast v ft1                          ft2
castIsValue v (NFFunc ft1)        (NFGType (GFunc y)) = Just $ VFuncCast v ft1                          (FuncType [] [] y TDyn TDyn)
castIsValue v (NFGType (GFunc x)) (NFFunc ft2)        = Just $ VFuncCast v (FuncType [] [] x TDyn TDyn) ft2
castIsValue v (NFGType (GFunc x)) (NFGType (GFunc y)) = Just $ VFuncCast v (FuncType [] [] x TDyn TDyn) (FuncType [] [] y TDyn TDyn)
castIsValue _ _ _ = Nothing

reduceCast' :: Value -> NFType -> NFType -> Maybe Value
reduceCast' v t NFDyn =
  if t `isSubtypeOf` NFDyn
  then Just v --Cast-Dyn-Dyn
  else do
    gt <- matchType t
    v' <- reduceCast v t (NFGType gt)
    Just $ C.trace "Factor-Left!" (VDynCast v' gt)
reduceCast' _ _ NFBot = Nothing -- Cast-Bot
reduceCast' (VDynCast v gt1) NFDyn (NFGType gt2) = if gt1 `isSubtypeOf` gt2 then Just v else Nothing -- Cast-Collapse/Cast-Collide
reduceCast' v NFDyn t = do
  gt <- matchType t
  let nfgt = NFGType gt
  let typeq = t `equalsType` gt
  if C.trace (show t ++ " == " ++ show gt ++ " = " ++ show typeq) (not typeq) then do
    v'  <- reduceCast v NFDyn nfgt
    v'' <- reduceCast v' nfgt t
    Just $ C.trace "Factor-Right!" v''  -- Factor-Right
  else
    Nothing
reduceCast' v (NFGType gt1) (NFGType gt2) = if gt1 `isSubtypeOf` gt2 then Just v else Nothing
reduceCast' _ _ _ = Nothing

toNFPair :: NFType -> NFType
toNFPair (NFGType (GPair s)) = NFPair (FuncType [] [] s TDyn TDyn)
toNFPair t = t

reducePairCast :: Value -> NFType -> NFType -> IO (Maybe Value)
reducePairCast (VPair v w) (NFPair (FuncType penv aenv s t1 t2)) (NFPair (FuncType penv' aenv' s' t1' t2')) = do
  mv' <- reduceComponent v (penv, aenv, t1) (penv', aenv', t1')
  C.traceIO $ "Reduce cast of left Value(" ++ show v ++ ") from NFType(" ++ show t1 ++ ") to NFType(" ++ show t1' ++ ") returning: " ++ show mv'
  case mv' of
    Nothing -> return Nothing
    Just v' -> do
      mw' <- reduceComponent w ((s, v) : penv, aenv, t2) ((s', v') : penv', aenv', t2')
      C.traceIO $ "Reduce cast of right Value(" ++ show w ++ ") from NFType(" ++ show t2 ++ ") to NFType(" ++ show t2' ++ ") returning: " ++ show mw'
      return $ liftM2 VPair mv' mw'
  where
    reduceComponent :: Value -> (PEnv, NREnv, Type) -> (PEnv, NREnv, Type) -> IO (Maybe Value)
    reduceComponent v (penv, aenv, t) (penv', aenv', t') = do
      nft  <- R.runReaderT (evalType t)  (penv,  aenv)
      nft' <- R.runReaderT (evalType t') (penv', aenv')
      return $ reduceCast v nft nft'
reducePairCast _ _ _ = return Nothing

equalsType :: NFType -> GType -> Bool
equalsType (NFFunc (FuncType _ _ x TDyn TDyn)) (GFunc x') = x == x'
equalsType (NFPair (FuncType _ _ x TDyn TDyn)) (GPair x') = x == x'
equalsType (NFGType gt1) gt2 = gt1 == gt2
equalsType _ _ = False

matchType :: NFType -> Maybe GType
matchType = \case
  NFFunc (FuncType _ _ x _ _) -> Just $ GFunc x
  NFPair (FuncType _ _ x _ _) -> Just $ GPair x
  NFGType gt -> Just gt
  _ -> Nothing