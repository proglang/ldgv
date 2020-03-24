{-# LANGUAGE TypeApplications #-}
module Interpreter where
import qualified Debug.Trace as D
import qualified Tokens as T
import qualified Grammar as G
import Syntax
import qualified Typing as Ty
import qualified Kinds as K
import qualified TCSubtyping as TS
import qualified TCTyping as TT
import Control.Concurrent.Chan as C
import Control.Concurrent (forkIO)
import ProcessEnvironment
import qualified Control.Monad as M
import Control.Monad.State as S


-- | interpret the "main" value in an ldgv file given over stdin
interpret :: String -> IO ()
interpret filename = do
  -- gather Type and Function definitions
  s <- readFile filename
  let tokens = T.alexScanTokens s
  let cmds = G.parseCalc tokens
  let dfuns = filter isDFun cmds where
        isDFun (DFun _ _ _ _) = True
        isDFun _ = False
  let dtypes = filter isDType cmds where
        isDType (DType _ _ _ _) = True
        isDType _ = False
  let penv = createPEnv $ dtypes ++ dfuns

  print dfuns
  print dtypes

  -- find the main DFun
  let m = case lookup "main" penv of
                    Just val -> val
                    -- TODO: fail if no main found
  let d = case m of
        (VDecl decl)-> decl
  -- and interpret the declaration
  x <- S.runStateT (evalDFun d) penv

  putStrLn $ "Interpretation of main resulted in " ++ show (fst x) ++ "\nand State"
  mapM_ print $ snd x

-- | interpret a DFun (Function declaration)
evalDFun :: Decl -> InterpretM
evalDFun decl@(DFun name [] expression _) = interpret' expression  -- a Declaration without free variables can be just interpreted
evalDFun decl@(DFun name ((_, id, _):binds) e mty) = do
                          let decl' = DFun name binds e mty
                          return $ VFun (\arg -> do
                                                  -- bind the identifier to the given value at application
                                                  createPMEntry (id, arg)
                                                  -- and evaluate the rest of the declaration
                                                  evalDFun decl') 

-- | interpret a single Expression
interpret' :: Exp ->  InterpretM
interpret' e =
  M.ap
  (return (\val -> D.trace ("Leaving interpretation of " ++ show e ++ " with value " ++ show val) val)) $
  case D.trace ("Invoking interpretation on " ++ show e) $ e of
  Int i -> return $ VInt i
  Nat i -> return $ VInt i
  Succ e -> mathHelper (+) (Int 1) e
  -- NatRec e1 e2 Ident TIdent Ident Type e3 -> do -- TODO
  Lam m i t e -> do
                 env <- get 
                 let f = \arg -> do
                                 liftIO $ S.evalStateT (interpret' e) (extendEnv (i, arg) env) 
                 return $ VFun f 
  Unit -> return VUnit
  Var s -> pmlookup s
  Lab s -> return $ VLabel s
  Let s e1 e2 -> do
      v  <- interpret' e1
      createPMEntry (s, v)
      interpret' e2
  Plus e1 e2 ->  mathHelper (+) e1 e2
  Minus e1 e2 ->  mathHelper (-) e1 e2
  Times e1 e2 ->  mathHelper (*) e1 e2
  Div e1 e2 ->  mathHelper quot e1 e2
  Negate e1 ->  mathHelper (-) (Int 0) e1
  App e1 e2 -> do
      _ <- liftIO $ putStrLn $ "Arguments for " ++ show e1 ++ " are: "  ++ show e2
      -- interpret e1 first, because the innermost application
      -- is the function with its first argument
      v1 <- interpret' e1
      arg <- interpret' e2
      -- check if the variable refers to a function declaration 
      case v1 of
          VDecl d -> do
            res <- (evalDFun d)
            case res of
              (VFun f) -> do
                f arg
          VFun f -> do
            f arg
          _ -> do
            fail ("Trying to Apply " ++ show e2 ++ " to " ++ show e1)
  Pair mul s e1 e2 -> do
      v1 <- interpret' e1
      v2 <- interpret' e2
      let val = VPair v1 v2
      createPMEntry (s , val)
      return val
  LetPair s1 s2 e1 e2 -> do
      v <- interpret' e1
      _ <- (case v of
        (VPair v1 v2) -> do
                createPMEntry (s1, v1)
                createPMEntry (s2, v2))
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
                      putStrLn "Ran a forked operation")
      return VUnit
  New t -> do
    r <- liftIO C.newChan
    w <- liftIO C.newChan
    return $ VPair (VChan r w) (VChan w r)
  Send e -> do
      v <- interpret' e
      case v of
        (VChan _ c) -> return $ VFun (\arg -> do
                                        liftIO $ putStrLn $ "Sending Value " ++ show arg ++ " on Channel " ++ show v
                                        liftIO (C.writeChan c arg)
                                        return v)
  Recv e -> do
      v <- interpret' e
      case v of
        (VChan c _) -> do
          val <- liftIO $ C.readChan c
          liftIO $ putStrLn $ "Read " ++ show val ++ " from Chan "
          return $ VPair val v
  Case e cases -> do
      v <- interpret' e
      case v of
        (VLabel s) -> do
          let e1 = lookup s cases
          case e1 of
            Just e' -> interpret' e'
  e -> do fail ("Expression " ++ show e ++ " not implemented")


-- | helper function for mathematical operations
mathHelper op e1 e2 = do
    v1 <- interpret' e1
    v2 <- interpret' e2
    liftIO $ putStrLn $ "MathHelper works on " ++ show v1 ++ " and " ++ show v2
    return $ case (v1, v2) of
      (VInt a, VInt b) -> VInt (op a b)


createPEnv :: [Decl] -> PEnv
createPEnv = map createEntry 

createEntry :: Decl -> PEnvEntry
createEntry d@(DType str mult kind typ) = (str, VType typ)
createEntry d@(DFun str args e mt) = (str, VDecl d)
