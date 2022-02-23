{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Interpreter (interpret, evalDFun, createPEnv) where

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
  case penvlookup "main" penv of
    Just (VDecl decl) -> R.runReaderT (evalDFun decl) penv
    _ -> throw $ LookupException "main"

-- | interpret a DFun (Function declaration)
evalDFun :: Decl -> InterpretM Value
evalDFun decl@(DFun name [] e _) = interpret' e  -- a Declaration without free variables can be just interpreted
evalDFun decl@(DFun name binds e _) =
  let lambda = foldr (\(mul, id, ty) -> Lam mul id ty) e binds
  in interpret' lambda

-- | interpret a single Expression
interpret' :: Exp ->  InterpretM Value
interpret' e = ask >>= \penv ->
  M.ap
  (return $ \val -> C.trace ("Leaving interpretation of " ++ pshow e ++ " with value " ++ show val) val) $
  (C.trace ("Invoking interpretation on " ++ pshow e) . eval) e

eval :: Exp -> InterpretM Value
eval = \case
  Succ e -> interpretMath $ Add (Lit (LInt 1)) e
  Rec f x e1 e0 -> ask >>= \env -> return $ VRec env f x e1 e0
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
    env <- ask
    case penvlookup i env of
      Just v -> R.local (extendEnv i v) (interpret' e)
      Nothing -> return $ VFunc env i e
  cast@(Cast e t1 t2) -> do
    C.traceIO $ "Interpreting cast expression: " ++ pshow cast
    v <- interpret' e
    C.traceIO ("Evaluating expression " ++ show e ++ " to value " ++ show v)
    nft1 <- evalType t1
    C.traceIO $ "Evaluating type " ++ show t1 ++ " to normal form " ++ show nft1
    nft2 <- evalType t2
    C.traceIO $ "Evaluating type " ++ show t2 ++ " to normal form " ++ show nft2
    case (v, nft1, nft2) of
      (pair@VPair {}, from@NFPair {}, to@NFPair {}) -> do
        C.traceIO $ "Interpreting pair cast expression: Value(" ++ show pair ++ ") NFType(" ++ show from ++ ") NFType(" ++ show to ++ ")"
        v' <- lift $ reducePairCast pair from to
        maybe (blame cast) return v'
      _ -> maybe (blame cast) return (reduceCast v nft1 nft2)
  Var s -> pmlookup s
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
  Send e -> do
    v <- interpret' e
    case v of
      (VChan _ c) -> return $ VFun (\arg -> do
        liftIO $ C.traceIO $ "Sending Value " ++ show arg ++ " on Channel " ++ show v
        liftIO (Chan.writeChan c arg)
        return v)
  Recv e -> do
    v <- interpret' e
    case v of
      (VChan c _) -> do
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
interpretApp _ (VFun f) w = f w
interpretApp _ (VFunc env s exp) w = R.local (\_ -> extendEnv s w env) (interpret' exp)
interpretApp e (VFuncCast v (FuncType penv _ s t1 t2) (FuncType penv' _ s' t1' t2')) w' = do
  penv0 <- ask
  let
    interpretAppCast :: IO Value
    interpretAppCast = do
      C.traceIO ("Attempting function cast in application (" ++ show v ++ ") with (" ++ show w' ++ ")")
      nft1  <- R.runReaderT (evalType t1)  penv
      nft1' <- R.runReaderT (evalType t1') penv'
      w <- maybe (blame e) return (reduceCast w' nft1 nft1')
      nft2' <- R.runReaderT (evalType t2') (extendEnv s' w' penv')
      nft2  <- R.runReaderT (evalType t2) (extendEnv s w penv)
      u  <- R.runReaderT (interpretApp e v w) penv0
      u' <- maybe (blame e) return (reduceCast u nft2 nft2')
      C.traceIO ("Function cast in application results in: " ++ show u')
      return u'
  lift interpretAppCast
interpretApp _ rec@(VRec env f x e1 e0) (VInt n)
  | n  < 0 = throw $ RecursorNotNatException (toInteger n)
  | n == 0 = interpret' e0
  | n  > 0 = let env' = extendEnv f rec (extendEnv x (VInt (n-1)) env)
             in R.local (\_ -> env') (interpret' e1)
interpretApp e _ _ = throw $ ApplicationException e

interpretLit :: Literal -> Value
interpretLit = \case
  LInt i -> VInt i
  LNat n -> VInt n
  LLab l -> VLabel l
  LDouble d -> VDouble d
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
  TUnit -> return NFUnit
  TInt -> return NFInt
  TDouble -> return NFDouble
  TBot -> return NFBot
  TDyn -> return NFDyn
  TNat -> return NFNat
  TNatLeq n -> return $ NFNatLeq n
  TNatRec e t1 tid t2 -> do
    v <- interpret' e
    case v of
      VInt 0 -> evalType t2
      VInt n -> if n < 0
        then throw $ RecursorNotNatException (toInteger n)
        else R.local (id) (evalType t2)
      _ -> throw $ RecursorException "Evaluation of 'natrec x...' must yield Nat value"
  TName _ s -> pmlookup s >>= \case
    (VType t) -> evalType t
    _ -> throw $ LookupException s
  TLab ls -> return $ NFLabel $ labelsFromList ls
  TFun  _ s t1 t2 -> ask >>= \penv -> return $ NFFunc $ FuncType penv [] s t1 t2
  TPair _ s t1 t2 -> ask >>= \penv -> return $ NFPair $ FuncType penv [] s t1 t2
  TCase exp labels -> interpret' exp >>= \(VLabel l) ->
    let entry = find (\(l', _) -> l == l') labels
    in maybe (return NFBot) (evalType . snd) entry
  t -> throw $ TypeNotImplementedException t

reduceCast :: Value -> NFType -> NFType -> Maybe Value
reduceCast v t1 t2 = castIsValue v t1 t2
                 <|> reduceCast' v t1' t2'
  where t1' = packGType t1
        t2' = packGType t2

-- Cast-Is-Value: return correct value if arguments already form a value
castIsValue :: Value -> NFType -> NFType -> Maybe Value
castIsValue v t NFDyn = do
  gt <- matchType t
  if t `equalsType` gt then Just $VDynCast v gt else Nothing
castIsValue v (NFFunc ft1) (NFFunc ft2) = Just $ VFuncCast v ft1 ft2
castIsValue _ _ _ = Nothing

reduceCast' :: Value -> NFType -> NFType -> Maybe Value
reduceCast' v t NFDyn = maybe castDynDyn factorLeft (matchType t)
  where
    castDynDyn = if t `isSubtypeOf` NFDyn then Just v else Nothing  -- Cast-Dyn-Dyn
    factorLeft gt = if let typeq = t `equalsType` gt in C.trace (show t ++ " == " ++ show gt ++ " = " ++ show typeq) (not typeq)
      then reduceCast v t (NFGType gt) >>= \v' -> Just $ VDynCast v' gt -- Factor-Left
      else castDynDyn
reduceCast' _ _ NFBot = Nothing -- Cast-Bot
reduceCast' v (NFGType gt1) (NFGType gt2) = if gt1 `isSubtypeOf` gt2 then Just v else Nothing -- Cast-Sub
reduceCast' (VDynCast v gt1) NFDyn (NFGType gt2) = if gt1 `isSubtypeOf` gt2 then Just v else Nothing -- Cast-Collapse/Cast-Collide
reduceCast' v NFDyn t = do
  gt <- matchType t
  let nfgt = NFGType gt
  let typeq = t `equalsType` gt
  if C.trace (show t ++ " == " ++ show gt ++ " = " ++ show typeq) (not typeq) then do
    v'  <- reduceCast v NFDyn nfgt
    v'' <- reduceCast v' nfgt t
    Just v''  -- Factor-Right
  else
    Nothing
reduceCast' v t1 t2 = do
  gt1 <- matchType t1
  gt2 <- matchType t2
  -- TODO: What do with gt1 and gt2?
  if gt1 `isSubtypeOf` gt2 then Just v else Nothing -- Cast-Sub/Cast-Fail

reducePairCast :: Value -> NFType -> NFType -> IO (Maybe Value)
reducePairCast (VPair v w) (NFPair (FuncType penv _ _ t1 t2)) (NFPair (FuncType penv' _ _ t1' t2')) = do
  v' <- reduceComponent v (penv, t1) (penv', t1')
  C.traceIO $ "Reduce cast of left Value(" ++ show v ++ ") from NFType(" ++ show t1 ++ ") to NFType(" ++ show t1' ++ ") returning: " ++ show v'
  w' <- reduceComponent w (penv, t2) (penv', t2')
  C.traceIO $ "Reduce cast of right Value(" ++ show w ++ ") from NFType(" ++ show t2 ++ ") to NFType(" ++ show t2' ++ ") returning: " ++ show w'
  return $ liftM2 VPair v' w'
  where
    reduceComponent :: Value -> (PEnv, Type) -> (PEnv, Type) -> IO (Maybe Value)
    reduceComponent v (penv, t) (penv', t') = do
      nft  <- R.runReaderT (evalType t)  penv
      nft' <- R.runReaderT (evalType t') penv'
      return $ reduceCast v nft nft'
reducePairCast _ _ _ = return Nothing

equalsType :: NFType -> GType -> Bool
equalsType NFUnit GUnit = True
equalsType (NFLabel ls1) (GLabel ls2) = ls1 == ls2
equalsType (NFFunc (FuncType _ _ _ TDyn TDyn)) GFunc = True
equalsType (NFPair (FuncType _ _ _ TDyn TDyn)) GPair = True
equalsType (NFGType gt1) gt2 = gt1 == gt2
equalsType NFNat GNat = True
equalsType (NFNatLeq n1) (GNatLeq n2) = n1 == n2
equalsType _ _ = False

matchType :: NFType -> Maybe GType
matchType = \case
  NFUnit -> Just GUnit
  NFLabel ls -> Just $ GLabel ls
  NFFunc FuncType {} -> Just GFunc
  NFPair FuncType {} -> Just GPair
  NFGType gt -> Just gt
  NFNat -> Just GNat
  NFNatLeq n -> Just $ GNatLeq n
  _ -> Nothing

packGType :: NFType -> NFType
packGType = \case
  NFUnit -> NFGType GUnit
  NFLabel ls -> NFGType $ GLabel ls
  NFFunc (FuncType _ _ _ TDyn TDyn) -> NFGType GFunc
  NFPair (FuncType _ _ _ TDyn TDyn) -> NFGType GPair
  NFNat -> NFGType GNat
  NFNatLeq n -> NFGType $ GNatLeq n
  nfgt@(NFGType _) -> nfgt  -- avoid recursion
  t -> t