module Typechecker (typecheck, Options(..)) where

import Control.Monad
import Control.Monad.Except
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import qualified Kinds as K
import qualified Syntax as G
import qualified PrettySyntax as PS
import qualified TCSubtyping as TS
import qualified TCTyping as TT
import qualified TCXMonad as TC
import Config as C

data Seen = SeenSig G.Type | SeenDef
  deriving (Eq)

data Options = Options
  { gradual :: Bool
  }

-- | Typecheck a list of declarations.
--                      Left -> Error  Ok <- Right
typecheck :: Options -> [G.Decl] -> Either String ()
typecheck tcOptions decls = do
  C.traceM "-------- Running Typecheck Request --------"
  exec tcOptions Map.empty [] [] decls

exec :: Options -> Map G.Ident Seen -> [G.TEnvEntry] -> G.KEnv -> [G.Decl] -> Either String ()
exec _ _ _ _ [] = return ()
exec tcOptions seendIds tenv kenv (cmd:cmds) = execCmd cmd
  where
    execCmd :: G.Decl -> Either String ()
    execCmd (G.DSub ty1 ty2) = do
      C.traceM "--- subtyping ---"
      void $ runTC tcOptions (TS.subtype tenv ty1 ty2) kenv
      exec tcOptions seendIds tenv kenv cmds
    execCmd (G.DEqv ty1 ty2) = do
      C.traceM "--- equivalence ---"
      void $ runTC tcOptions (TS.eqvtype tenv ty1 ty2) kenv
      exec tcOptions seendIds tenv kenv cmds
    execCmd (G.DSubst x e1 e2) = do
      C.traceShowM $ G.subst x e1 e2
      exec tcOptions seendIds tenv kenv cmds
    execCmd (G.DSig x m ty) = do
      C.traceM ("--- signature: " ++ x ++ " ---")
      case Map.lookup x seendIds of
        Just (SeenSig _) ->
          throwError $ "duplicate signatures for ‘" ++ x ++ "’"
        Just SeenDef ->
          throwError $ "signature for ‘" ++ x ++ "’ given after its definition"
        Nothing ->
          exec tcOptions (Map.insert x (SeenSig ty) seendIds) ((x,(m, ty)) : tenv) kenv cmds
    execCmd (G.DFun f binds e mty) = do
      traceM ("--- type checking: " ++ f ++ " ---")
      let buildFunction c = foldr (\(m, v, ty) -> c m v ty)
          buildty = buildFunction G.TFun
          e' = buildFunction G.Lam e binds
      (tenv', cmds') <- case (Map.lookup f seendIds, mty) of
        (Nothing, Nothing) -> do
          -- Synthesize the type of the definition.
          (ty, _) <- runTC tcOptions (TT.tySynthUnfold tenv e') kenv
          return ((f, (K.Many, ty)) : tenv, cmds)
        (Nothing, Just ty) -> do
          -- Check the definition against the given type.
          let fty = buildty ty binds
              tenv' = (f, (K.Many, fty)) : tenv
          void $ runTC tcOptions (TT.tyCheck tenv' e' fty) kenv
          return (tenv', cmds)
        (Just (SeenSig sigTy), _) -> do
          -- Check the definition against the signatures type.
          void $ runTC tcOptions (TT.tyCheck tenv e' sigTy) kenv
          -- If mty is given, check type equivalence between it and sigTy
          -- next.
          let eqv rty = G.DEqv sigTy (buildty rty binds)
          return (tenv, maybe id ((:) . eqv) mty cmds)
        (Just SeenDef, _) ->
          throwError $ "duplicate definition for ‘" ++ f ++ "’"
      exec tcOptions (Map.insert f SeenDef seendIds) tenv' kenv cmds'
    execCmd (G.DType tid _m k ty) = do
      traceM ("--- type declaration: " ++ tid ++ " ---")
      -- TODO: in general, we need to wait with this check until all types are declared
      let kenv' = (tid, (ty, k)):kenv
      -- C.printDebug (Ty.kiCheck tenv ty k)
      runTC tcOptions (TT.kiCheck tenv ty k) kenv'
      exec tcOptions seendIds tenv kenv' cmds
    execCmd (G.DAssume _ _) = do
      exec tcOptions seendIds tenv kenv cmds

runTC :: (PS.Pretty a, PS.Pretty w) => Options -> TC.M TS.ReadOnly TS.Caches w a -> G.KEnv -> Either String a
runTC tcOptions m kenv =
  let mReadOnly = TS.ReadOnly { TS.kenv = kenv, TS.gradual = gradual tcOptions } in
  case fst $ TC.runM m mReadOnly TS.initCaches of
    Left err -> throwError err
    Right res -> fst res <$ traceSuccess res
