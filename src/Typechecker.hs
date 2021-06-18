{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
module Typechecker (typecheck) where

import Control.Monad
import Control.Monad.Except
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import qualified Kinds as K
import qualified Subtyping as S
import qualified Syntax as G
import qualified Syntax.Pretty as PS
import qualified TCSubtyping as TS
import qualified TCTyping as TT
import qualified TCXMonad as TC
import Config as C

data Seen = SeenSig G.Type | SeenDef
  deriving (Eq)

-- | Typecheck a list of declarations.
typecheck :: [G.Decl] -> Either String ()
typecheck decls = do
  C.traceM "-------- Running Typecheck Request --------"
  exec Map.empty [] [] decls
    where
      runTC :: (PS.Pretty a, PS.Pretty w) => TC.M r TS.Caches w a -> r -> Either String a
      runTC m kenv =
        case fst $ TC.runM m kenv TS.initCaches of
          Left err -> throwError err
          Right res -> fst res <$ traceSuccess res

      exec :: Map G.Ident Seen -> [G.TEnvEntry] -> G.KEnv -> [G.Decl] -> Either String ()
      exec _ _ _ [] = return ()
      exec seendIds tenv kenv (cmd:cmds) =
        case cmd of
          G.DSub ty1 ty2 -> do
            C.traceM "--- subtyping ---"
            void $ runTC (TS.subtype tenv ty1 ty2) kenv
            exec seendIds tenv kenv cmds

          G.DEqv ty1 ty2 -> do
            C.traceM "--- equivalence ---"
            void $ runTC (TS.eqvtype tenv ty1 ty2) kenv
            exec seendIds tenv kenv cmds

          G.DSubst x e1 e2 -> do
            C.traceShowM $ G.subst x e1 e2
            exec seendIds tenv kenv cmds

          G.DLub _ ty1 ty2 -> do
            C.traceShowM (S.lub tenv ty1 ty2)
            exec seendIds tenv kenv cmds

          G.DGlb _ ty1 ty2 -> do
            C.traceShowM (S.glb tenv ty1 ty2)
            exec seendIds tenv kenv cmds

          G.DSig x m ty -> do
            C.traceM ("--- signature: " ++ x ++ " ---")
            case Map.lookup x seendIds of
              Just (SeenSig _) ->
                throwError $ "duplicate signatures for ‘" ++ x ++ "’"
              Just SeenDef ->
                throwError $ "signature for ‘" ++ x ++ "’ given after its definition"
              Nothing ->
                exec (Map.insert x (SeenSig ty) seendIds) ((x,(m, ty)) : tenv) kenv cmds 

          G.DFun f binds e mty -> do
            traceM ("--- type checking: " ++ f ++ " ---")

            let buildFunction c = foldr (\(m, v, ty) -> c m v ty)
                buildty = buildFunction G.TFun
                e' = buildFunction G.Lam e binds
            
            (tenv', cmds') <- case (Map.lookup f seendIds, mty) of
              (Nothing, Nothing) -> do
                -- Synthesize the type of the definition.
                (ty, _) <- runTC (TT.tySynthUnfold tenv e') kenv
                return ((f, (K.Many, ty)) : tenv, cmds)

              (Nothing, Just ty) -> do
                -- Check the definition against the given type.
                let fty = buildty ty binds
                    tenv' = (f, (K.Many, fty)) : tenv
                void $ runTC (TT.tyCheck tenv' e' fty) kenv
                return (tenv', cmds)

              (Just (SeenSig sigTy), _) -> do
                -- Check the definition against the signatures type.
                void $ runTC (TT.tyCheck tenv e' sigTy) kenv

                -- If mty is given, check type equivalence between it and sigTy
                -- next.
                let eqv rty = G.DEqv sigTy (buildty rty binds)
                return (tenv, maybe id ((:) . eqv) mty cmds)

              (Just SeenDef, _) ->
                throwError $ "duplicate definition for ‘" ++ f ++ "’"

            exec (Map.insert f SeenDef seendIds) tenv' kenv cmds'

          G.DType tid _m k ty -> do
            traceM ("--- type declaration: " ++ tid ++ " ---")
            -- TODO: in general, we need to wait with this check until all types are declared
            let kenv' = (tid, (ty, k)):kenv
            -- C.printDebug (Ty.kiCheck tenv ty k)
            runTC (TT.kiCheck tenv ty k) kenv'
            exec seendIds tenv kenv' cmds

          G.DAssume _ _ -> do
            exec seendIds tenv kenv cmds
