{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Interpreter
  ( interpret
  , interpretDecl
  , evalType
  , InterpreterException(..)
  ) where

import qualified Config as C
import Syntax
import PrettySyntax
import qualified Control.Concurrent.Chan as Chan
import qualified Control.Concurrent.MVar as MVar
import Network.Socket
-- import qualified Network.Socket as NSocket
import Control.Concurrent (forkIO)
import Data.Foldable (find)
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Map as Map
import ProcessEnvironment
import Networking.NetworkingMethod.NetworkingMethodCommon
import qualified Control.Monad as M
import Control.Monad.Reader as R
import Control.Applicative ((<|>))
import Control.Exception
import Kinds (Multiplicity(..))

import qualified ValueParsing.ValueTokens as VT
import qualified ValueParsing.ValueGrammar as VG
import qualified Networking.Common as NC
import qualified Networking.Client as NClient

import Network.Run.TCP
import qualified Networking.Server as NS

import Networking.UserID as UserID

import Control.Concurrent
import qualified Networking.UserID as UserID

import qualified Networking.Messages as Messages
import qualified Networking.DirectionalConnection as DC
import qualified Networking.NetworkConnection as NCon
-- import ProcessEnvironment (CommunicationChannel(CommunicationChannel, ccChannelState, ccPartnerUserID), ConnectionInfo (ciReadChannel, ciWriteChannel))
-- import ProcessEnvironment
import qualified Control.Concurrent as MVar
import ProcessEnvironment
import ProcessEnvironmentTypes
import Networking.NetworkConnection (NetworkConnection(ncPartnerUserID))
import qualified Control.Concurrent as MVar
import qualified Control.Concurrent as MVar
import qualified Control.Concurrent as MVar
import qualified Control.Concurrent as MVar
import qualified Control.Concurrent as MVar
import qualified Control.Concurrent as MVar
-- import qualified Networking.NetworkConnection as NCon
-- import qualified Networking.NetworkConnection as NCon

import qualified Data.Bifunctor
-- import qualified Networking.NetworkingMethod.Stateless as NetMethod
-- import qualified Networking.NetworkingMethod.Fast as NetMethod

data InterpreterException
  = MathException String
  | LookupException String
  | CastException Exp
  | ApplicationException Exp
  | RecursorException String
  | RecursorNotNatException
  | NotImplementedException Exp
  | TypeNotImplementedException Type
  | DeserializationException String
  | NotAnExpectedValueException String Value
  | CommunicationPartnerNotFoundException String
  | VChanIsUsedException String
  deriving Eq

instance Show InterpreterException where
  show = \case
    (MathException s) -> "MathException: " ++ s
    (LookupException s) -> "LookupException: Lookup of '" ++ s ++ "' did not yield a value"
    (CastException exp) -> "CastException: (" ++ pshow exp ++ ") failed"
    (ApplicationException exp) -> "ApplicationException: expression '" ++ pshow exp ++ "' not allowed"
    (RecursorException s) -> "RecursorException: " ++ s
    RecursorNotNatException -> "Recursor only works on natural numbers"
    (NotImplementedException exp) -> "NotImplementedException: " ++ pshow exp
    (TypeNotImplementedException typ) -> "TypeNotImplementedException: " ++ pshow typ
    (DeserializationException err) -> "DeserializationException: " ++ err
    (NotAnExpectedValueException expected val) -> "NotAnExpectedValueException: This expresion: (" ++ pshow val ++ ") is not of type: " ++ expected
    (CommunicationPartnerNotFoundException partner) -> "CommunicationPartnerNotFoundException: Partner:" ++ partner ++ " not found"
    (VChanIsUsedException chan) -> "VChanIsUsedException: VChan " ++ chan ++ " is already used!"

instance Exception InterpreterException

blame :: Exp -> a
blame exp = throw $ CastException exp

-- | interpret the "main" value in an ldgv file given over stdin
interpret :: [Decl] -> IO Value
interpret decls = do
  sockets <- MVar.newEmptyMVar
  activeConnections <- NC.createActiveConnections
  MVar.putMVar sockets Map.empty
  R.runReaderT (interpretDecl decls) ([], (sockets, activeConnections))

interpretDecl :: [Decl] -> InterpretM Value
interpretDecl (DFun "main" _ e _:_) = interpret' e
interpretDecl (DFun name [] e _:decls) = interpret' e >>= \v -> local (Data.Bifunctor.first (extendEnv name v)) (interpretDecl decls)
interpretDecl (DFun name binds e _:decls) =
  let lambda = foldr (\(mul, id, ty) -> Lam mul id ty) e binds
  in interpret' lambda >>= \v -> local (Data.Bifunctor.first (extendEnv name v)) (interpretDecl decls)
interpretDecl (DType name _ _ t:decls) = local (Data.Bifunctor.first (extendEnv name $ VType t)) (interpretDecl decls)
interpretDecl (_:decls) = interpretDecl decls
interpretDecl [] = throw $ LookupException "main"

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
        R.local (Data.Bifunctor.first (extendEnv i1 (VInt 0) . extendEnv i2 zero)) (interpret' e3)
      VInt n -> do
        -- interpret the n-1 case i2 and add it to the env
        -- together with n before interpreting the body e3
        let lowerEnv = extendEnv i1 (VInt $ n-1)
        lower <- R.local (Data.Bifunctor.first lowerEnv) (interpret' $ NatRec (Var i1) e2 i1 t1 i2 t e3)
        R.local (Data.Bifunctor.first (extendEnv i2 lower . lowerEnv)) (interpret' e3)
      _ -> throw $ RecursorException "Evaluation of 'natrec x...' must yield Nat value"
  NewNatRec f n tid ty ez x es -> ask >>= \(env, _) -> return $ VNewNatRec env f n tid ty ez x es
  Lam _ i _ e -> ask >>= \(env, sock) -> return $ VFunc env i e
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
        (env, (sockets, activeConnections)) <- ask
        v' <- lift $ reducePairCast sockets activeConnections v (toNFPair nft1) (toNFPair nft2)
        maybe (blame cast) return v'
      _ -> let v' = reduceCast v nft1 nft2 in maybe (blame cast) return v'
  Var s -> ask >>= \(env, _) -> maybe (throw $ LookupException s) (liftIO . pure) (lookup s env)
  Let s e1 e2 -> interpret' e1 >>= \v -> R.local (Data.Bifunctor.first (extendEnv s v)) (interpret' e2)
  Math m -> interpretMath m
  Lit l -> return (interpretLit l)
  e@(App e1 e2) -> do
    liftIO $ C.traceIO $ "Arguments for (" ++ pshow e1 ++ ") are ("  ++ pshow e2 ++ ")"
    val <- interpret' e1
    arg <- interpret' e2
    interpretApp e val arg
  Pair mul s e1 e2 -> do
    v1 <- interpret' e1
    v2 <- R.local (Data.Bifunctor.first (extendEnv s v1)) (interpret' e2)
    return $ VPair v1 v2
  LetPair s1 s2 e1 e2 -> interpret' e1 >>= \(VPair v1 v2) -> R.local (Data.Bifunctor.first (extendEnv s2 v2 . extendEnv s1 v1)) (interpret' e2)
  fst@(Fst e) -> interpret' e >>= \(VPair s1 s2) -> return s1
  snd@(Snd e) -> interpret' e >>= \(VPair s1 s2) -> return s2
  Fork e -> do
    (penv, sock) <- ask
    liftIO $ forkIO (do
      res <- R.runReaderT (interpret' e) (penv, sock)
      C.traceIO "Ran a forked operation")
    return VUnit
  New t -> do
    r <- liftIO DC.newConnection
    w <- liftIO DC.newConnection
    nc1 <- liftIO $ NCon.newEmulatedConnection r w
    nc2 <- liftIO $ NCon.newEmulatedConnection w r
    mvar <- liftIO MVar.newEmptyMVar
    liftIO $ MVar.putMVar mvar Map.empty
    used1 <- liftIO MVar.newEmptyMVar
    liftIO $ MVar.putMVar used1 False
    used2 <- liftIO MVar.newEmptyMVar
    liftIO $ MVar.putMVar used2 False
    return $ VPair (VChan nc1 mvar used1) $ VChan nc2 mvar used2
  Send e -> VSend <$> interpret' e -- Apply VSend to the output of interpret' e
  Recv e -> do
    interpret' e >>= \v@(VChan ci mvar usedmvar) -> do
      used <- liftIO $ MVar.readMVar usedmvar
      if used then throw $ VChanIsUsedException $ show v else do
        let dcRead = NCon.ncRead ci
        valunclean <- liftIO $ DC.readUnreadMessageInterpreter dcRead
        (env, (sockets, activeConnections)) <- ask
        val <- liftIO $ NS.replaceVChanSerial activeConnections mvar valunclean
        liftIO $ C.traceIO $ "Read " ++ show val ++ " from Chan, over expression " ++ show e

        -- Disable the old channel and get a new one
        newV <- liftIO $ disableOldVChan v
        return $ VPair val newV
  End e -> do
    liftIO $ C.traceIO "Trying to close a connection"
    interpret' e >>= \v@(VChan ci mvar usedmvar) -> do
      used <- liftIO $ MVar.readMVar usedmvar
      if used then throw $ VChanIsUsedException $ show v else do
        liftIO $ C.traceIO $ "Trying to close connection with:" ++ (Data.Maybe.fromMaybe "" $ ncPartnerUserID ci)
        liftIO $ NClient.closeConnection ci

        -- Disable the channel
        _ <- liftIO $ disableOldVChan v
        return v
  Case e cases -> interpret' e >>= \(VLabel s) -> interpret' $ fromJust $ lookup s cases
  {- Create e -> do
    liftIO $ C.traceIO "Creating socket!"

    val <- interpret' e
    case val of
      VInt port -> do
        (_, (_, activeConnections)) <- ask
        (mvar, clientlist) <- liftIO $ NetMethod.acceptConversations activeConnections NS.handleClient port
        liftIO $ C.traceIO "Socket created"
        return $ VServerSocket mvar clientlist $ show port
      _ -> throw $ NotAnExpectedValueException "VInt" val
  -}
  Accept e t -> do
    liftIO $ C.traceIO "Accepting new client!"

    val <- interpret' e
    case val of
      VInt port -> do
        (env, (sockets, activeConnections)) <- ask
        (mvar, clientlist, ownport) <- liftIO $ NC.acceptConversations activeConnections NS.handleClient port sockets
        -- newuser <- liftIO $ Chan.readChan chan
        liftIO $ C.traceIO "Searching for correct communicationpartner"
        newuser <- liftIO $ NS.findFittingClient clientlist t -- There is still an issue
        liftIO $ C.traceIO "Client accepted"
        networkconnectionmap <- liftIO $ MVar.readMVar mvar
        case Map.lookup newuser networkconnectionmap of
          Nothing -> throw $ CommunicationPartnerNotFoundException newuser
          Just networkconnection -> do
            liftIO $ C.traceIO "Client successfully accepted!"
            used <- liftIO MVar.newEmptyMVar
            liftIO $ MVar.putMVar used False
            return $ VChan networkconnection mvar used
      _ -> throw $ NotAnExpectedValueException "VInt" val

  Connect e0 t e1 e2-> do
    r <- liftIO DC.newConnection
    w <- liftIO DC.newConnection
    liftIO $ C.traceIO "Client trying to connect"
    val <- interpret' e0
    case val of
      VInt port -> do
        (env, (sockets, activeConnections)) <- ask
        (networkconmapmvar, chan, ownport) <- liftIO $ NC.acceptConversations activeConnections NS.handleClient port sockets
        addressVal <- interpret' e1
        case addressVal of
          VString address -> do
            portVal <- interpret' e2
            case portVal of
              VInt port -> do
                liftIO $ NClient.initialConnect activeConnections networkconmapmvar address (show port) ownport t
              _ -> throw $ NotAnExpectedValueException "VInt" portVal
          _ -> throw $ NotAnExpectedValueException "VString" addressVal
      _ -> throw $ NotAnExpectedValueException "VInt" val
  e -> throw $ NotImplementedException e

-- Exp is only used for blame
interpretApp :: Exp -> Value -> Value -> InterpretM Value
interpretApp _ (VFunc env s exp) w = R.local (Data.Bifunctor.first (const $ extendEnv s w env)) (interpret' exp)
interpretApp e (VFuncCast v (FuncType penv s t1 t2) (FuncType penv' s' t1' t2')) w' = do
  (env0, socketMVar) <- ask
  let
    interpretAppCast :: IO Value
    interpretAppCast = do
      C.traceIO ("Attempting function cast in application (" ++ show v ++ ") with (" ++ show w' ++ ")")
      nft1  <- R.runReaderT (evalType t1)  (penv, socketMVar)
      nft1' <- R.runReaderT (evalType t1') (penv', socketMVar)
      w <- maybe (blame e) return (reduceCast w' nft1' nft1)
      nft2' <- R.runReaderT (evalType t2') (extendEnv s' w' penv', socketMVar)
      nft2  <- R.runReaderT (evalType t2)  (extendEnv s  w  penv, socketMVar)
      u  <- R.runReaderT (interpretApp e v w) (env0, socketMVar)
      u' <- maybe (blame e) return (reduceCast u nft2 nft2')
      C.traceIO ("Function cast in application results in: " ++ show u')
      return u'
  lift interpretAppCast
interpretApp e rec@(VRec env f n1 e1 e0) (VInt n)
  | n  < 0 = throw RecursorNotNatException
  | n == 0 = interpret' e0
  | n  > 0 = do
    let env' = extendEnv n1 (VInt (n-1)) (extendEnv f rec env)
    R.local (Data.Bifunctor.first (const env')) (interpret' e1)
interpretApp _ natrec@(VNewNatRec env f n1 tid ty ez y es) (VInt n)
  | n  < 0 = throw RecursorNotNatException
  | n == 0 = interpret' ez
  | n  > 0 = do
    let env' = extendEnv n1 (VInt (n-1)) (extendEnv f natrec env)
    R.local (Data.Bifunctor.first (const env')) (interpret' es)
-- interpretApp _ (VSend v@(VChan _ c handle _ _ _)) w = do
interpretApp _ (VSend v@(VChan cc _ usedmvar)) w = do
  used <- liftIO $ MVar.readMVar usedmvar
  if used then throw $ VChanIsUsedException $ show v else do
    (env, (sockets, activeConnections)) <- ask
    liftIO $ NClient.sendValue activeConnections cc w (-1)

    -- Disable old VChan
    newV <- liftIO $ disableOldVChan v
    return newV
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
  TInt -> return $ NFGType GInt
  TDouble -> return $ NFGType GDouble
  TString -> return $ NFGType GString
  TBot -> return NFBot
  TDyn -> return NFDyn
  TNat -> return $ NFGType GNat
  TNatLeq n -> return $ NFGType $ GNatLeq n
  TNatRec e t1 tid t2 -> do
    v <- interpret' e
    case v of
      VInt 0 -> evalType t1
      VInt n -> if n < 0
        then throw RecursorNotNatException
        else
          let lower = TNatRec (Lit $ LNat (n-1)) t1 tid t2
          in do
            R.local (Data.Bifunctor.first (extendEnv tid (VType lower))) (evalType t2)
      _ -> throw $ RecursorException "Evaluation of 'natrec x...' must yield Nat value"
  TName _ s -> ask >>= \(env, _) -> maybe (throw $ LookupException s) (\(VType t) -> evalType t) (lookup s env)
  TLab ls -> return $ NFGType $ GLabel $ labelsFromList ls
  TFun  m _ TDyn TDyn -> return $ NFGType $ GFunc m
  TFun  m s t1 t2 -> ask >>= \(env, _) -> return $ NFFunc $ FuncType env s t1 t2
  TPair _ TDyn TDyn -> return $ NFGType $ GPair
  TPair s t1 t2 -> ask >>= \(env, _) -> return $ NFPair $ FuncType env s t1 t2
  TCase exp labels -> interpret' exp >>= \(VLabel l) ->
    let entry = find (\(l', _) -> l == l') labels
    in maybe (return NFBot) (evalType . snd) entry
  t -> throw $ TypeNotImplementedException t

reduceCast :: Value -> NFType -> NFType -> Maybe Value
reduceCast v t1 t2 = castIsValue v t1 t2 <|> reduceCast' v t1 t2

-- Cast-Is-Value: return correct value if arguments already form a value
castIsValue :: Value -> NFType -> NFType -> Maybe Value
castIsValue v (NFGType gt) NFDyn = Just $ VDynCast v gt
castIsValue v (NFFunc ft1)        (NFFunc ft2)        = Just $ VFuncCast v ft1                       ft2
castIsValue v (NFFunc ft1)        (NFGType (GFunc _)) = Just $ VFuncCast v ft1                       (FuncType [] "x" TDyn TDyn)
castIsValue v (NFGType (GFunc _)) (NFFunc ft2)        = Just $ VFuncCast v (FuncType [] "x" TDyn TDyn) ft2
castIsValue v (NFGType (GFunc x)) (NFGType (GFunc y)) = Just $ VFuncCast v (FuncType [] "x" TDyn TDyn) (FuncType [] "y" TDyn TDyn)
castIsValue _ _ _ = Nothing

reduceCast' :: Value -> NFType -> NFType -> Maybe Value
reduceCast' v t NFDyn =
  if t `isSubtypeOf` NFDyn
  then Just v --Cast-Dyn-Dyn
  else do
    gt <- matchType t
    v' <- reduceCast v t (NFGType gt)
    Just (VDynCast v' gt) -- Factor-Left
reduceCast' _ _ NFBot = Nothing -- Cast-Bot
reduceCast' (VDynCast v gt1) NFDyn (NFGType gt2) = if gt1 `isSubtypeOf` gt2 then Just v else Nothing -- Cast-Collapse/Cast-Collide
reduceCast' v NFDyn t = do
  gt <- matchType t
  let nfgt = NFGType gt
  let typeq = t `equalsType` gt
  if not typeq then do
    v'  <- reduceCast v NFDyn nfgt
    v'' <- reduceCast v' nfgt t
    Just v''  -- Factor-Right
  else
    Nothing
reduceCast' v (NFGType gt1) (NFGType gt2) = if gt1 `isSubtypeOf` gt2 then Just v else Nothing
reduceCast' _ _ _ = Nothing

--- PT: this is weird
toNFPair :: NFType -> NFType
toNFPair (NFGType (GPair)) = NFPair (FuncType [] "x" TDyn TDyn)
toNFPair t = t

reducePairCast :: MVar.MVar (Map.Map Int ServerSocket) -> ActiveConnections -> Value -> NFType -> NFType -> IO (Maybe Value)
reducePairCast sockets activeConnections (VPair v w) (NFPair (FuncType penv s t1 t2)) (NFPair (FuncType penv' s' t1' t2')) = do
  mv' <- reduceComponent sockets activeConnections v (penv, t1) (penv', t1')
  case mv' of
    Nothing -> return Nothing
    Just v' -> do
      mw' <- reduceComponent sockets activeConnections w ((s, v) : penv, t2) ((s', v') : penv', t2')
      return $ liftM2 VPair mv' mw'
  where
    reduceComponent :: MVar.MVar (Map.Map Int ServerSocket) -> ActiveConnections -> Value -> (PEnv, Type) -> (PEnv, Type) -> IO (Maybe Value)
    reduceComponent sockets activeConnections v (penv, t) (penv', t') = do
      nft  <- R.runReaderT (evalType t)  (penv, (sockets, activeConnections))
      nft' <- R.runReaderT (evalType t') (penv', (sockets, activeConnections))
      return $ reduceCast v nft nft'
reducePairCast _ _ _ _ _ = return Nothing

equalsType :: NFType -> GType -> Bool
equalsType (NFFunc (FuncType _ _ TDyn TDyn)) (GFunc _) = True
equalsType (NFPair (FuncType _ _ TDyn TDyn)) (GPair) = True
equalsType (NFGType gt1) gt2 = gt1 == gt2
equalsType _ _ = False

matchType :: NFType -> Maybe GType
matchType = \case
  NFFunc (FuncType _ _ _ _) -> Just $ GFunc MMany
  NFPair (FuncType _ _ _ _) -> Just $ GPair
  NFGType gt -> Just gt
  _ -> Nothing
