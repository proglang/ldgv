{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Interpreter (interpret, evalDFun, createPEnv) where

import qualified Config as C
import Syntax
import Control.Concurrent.Chan as Chan
import Control.Concurrent (forkIO)
import Data.Maybe (fromMaybe)
import ProcessEnvironment
import qualified Control.Monad as M
import Control.Monad.State.Strict as S
import Control.Exception

data InterpreterException
  = MathException String
  | LookupException String
  | CastException
  | ApplicationException String
  | NotImplementedError Exp
  deriving Show

instance Exception InterpreterException

blame = throw CastException

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
    Just (VDecl decl) -> do
      endResult <- S.runStateT (evalDFun decl) penv
      return $ fst endResult
    _ -> throw $ LookupException "No 'main' value declaration found"

-- | interpret a DFun (Function declaration)
evalDFun :: Decl -> InterpretM Value
evalDFun decl@(DFun name [] expression _) = interpret' expression  -- a Declaration without free variables can be just interpreted
evalDFun decl@(DFun name ((_, id, _):binds) e mty) = do
  let decl' = DFun name binds e mty
  return $ VFun (\arg -> do
    -- bind the identifier to the given value at application
    createPMEntry (id, arg)
    -- and evaluate the rest of the declaration
    evalDFun decl')

-- | interpret a single Expression
interpret' :: Exp ->  InterpretM Value
interpret' e =
  M.ap
  (return (\val -> C.trace ("Leaving interpretation of " ++ show e ++ " with value " ++ show val) val)) $
  case C.trace ("Invoking interpretation on " ++ show e) e of
  Succ e -> interpretMath $ Add (Lit (LInt 1)) e
  exp@(NatRec e1 e2 i1 t1 i2 t e3) -> do
  -- returns a function indexed over e1 (should be a variable pointing to a Nat)
  -- e1 should be the recursive variable which gets decreased each time the
  -- non-zero case is evaluated
  -- e2 is the zero case
  -- e3 is the nonzero case
    i <- interpret' e1
    case i of
      VInt 0 -> interpret' e2
      VInt 1 -> do
        env <- get
        zero <- interpret' e2
        put $ (i1, VInt 0):(i2, zero):env
        interpret' e3
      VInt n -> do
        -- interpret the n-1 case i2 and add it to the env
        -- together with n before interpreting the body e3
        env <- get
        put $ (i1, VInt (n-1)):env
        lower <- interpret' $ NatRec (Var i1) e2 i1 t1 i2 t e3
        put $ (i1, VInt (n-1)):(i2, lower):env
        interpret' e3
  Lam m i t e -> do
    env <- get
    let f = \arg -> liftIO $ S.evalStateT (interpret' e) (extendEnv (i, arg) env)
    return $ VFun f
  Cast e t1 t2 -> do
    v <- interpret' e
    nft1 <- evalType t1
    nft2 <- evalType t2
    return $ fromMaybe blame (reduceCast v nft1 nft2)
  Var s -> pmlookup s
  Let s e1 e2 -> do
    v  <- interpret' e1
    createPMEntry (s, v)
    interpret' e2
  Math m -> interpretMath m
  Lit l -> return (interpretLit l)
  App e1 e2 -> do
    _ <- liftIO $ C.traceIO $ "Arguments for " ++ show e1 ++ " are: "  ++ show e2
    -- interpret e1 first, because the innermost application
    -- is the function with its first argument
    v1 <- interpret' e1
    arg <- interpret' e2
    -- check if the variable refers to a function declaration
    case v1 of
      VDecl d -> do
        res <- evalDFun d
        case res of (VFun f) -> f arg
      VFun f -> f arg
      _ -> throw $ ApplicationException ("Trying to Apply " ++ show e2 ++ " to " ++ show e1)
  Pair mul s e1 e2 -> do
    v1 <- interpret' e1
    v2 <- interpret' e2
    let val = VPair v1 v2
    createPMEntry (s , val)
    return val
  LetPair s1 s2 e1 e2 -> do
    v <- interpret' e1
    case v of
      (VPair v1 v2) -> do
        createPMEntry (s1, v1)
        createPMEntry (s2, v2)
    interpret' e2
  Fst e -> do
    v <- interpret' e
    case v of
      (VPair s1 s2) -> return s1
  Snd e -> do
    v <- interpret' e
    case v of
      (VPair s1 s2) -> return s2
  Fork e -> do
    penv <- get
    liftIO $ forkIO (do
      res <- S.runStateT (interpret' e) penv
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
    v <- interpret' e
    case v of
      (VLabel s) -> do
        let e1 = lookup s cases
        case e1 of
          Just e' -> interpret' e'
  e -> throw $ NotImplementedError e

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

createPEnv :: [Decl] -> PEnv
createPEnv = map createEntry

createEntry :: Decl -> PEnvEntry
createEntry d@(DType str mult kind typ) = (str, VType typ)
createEntry d@(DFun str args e mt) = (str, VDecl d)

-- TODO: What about built-in types without normal form equivalent?
evalType :: Type -> InterpretM NFType
evalType = \case
  TBot -> return NFBot
  TDyn -> return NFDyn
  TUnit -> return NFUnit
  TLab labels -> return $ NFLab labels
  TName _ s -> pmlookup s >>= (\case
    (VType t) -> evalType t
    _ -> throw $ LookupException ("Type lookup of '" ++ s ++ "' did not yield a type value"))

reduceCast :: Value -> NFType -> NFType -> Maybe Value
reduceCast VUnit _ _ = Just VUnit
reduceCast v t1 NFDyn = Just $ VDynCast v t1
reduceCast v t1 NFBot = Nothing
reduceCast v t1 t2 = if t1 == t2 then Just v else Nothing
