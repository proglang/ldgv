module Main where

import qualified Tokens as T
import qualified Grammar as G
import qualified Syntax as G
import qualified Subtyping as S
import qualified Typing as Ty
import qualified Kinds as K

import qualified TCXMonad as TC
import qualified TCSubtyping as TS
import qualified TCTyping as TT

import PrettySyntax

printResult :: (Pretty a, Pretty b) => (Either a b, s) -> IO ()
printResult (Left a, _) =
  putStrLn ("Error: " ++ pshow a)
printResult (Right b, _) =
  putStrLn ("Success: " ++ pshow b)

main :: IO ()
main = do
  s <- getContents
  let ts = T.alexScanTokens s
  -- print ts
  let cmds = G.parseCalc ts
  print cmds
  putStrLn "-------- running request --------"
  exec [] [] cmds
    where
      exec tenv kenv [] = return ()
      exec tenv kenv (cmd:cmds) = 
        case cmd of
          G.DSub ty1 ty2 -> do
            putStrLn "--- subtyping ---"
            printResult (TC.runM (TS.subtype tenv ty1 ty2) kenv TS.initCaches)
            exec tenv kenv cmds
            -- print (S.subtype tenv ty1 ty2)
          G.DEqv ty1 ty2 -> do
            putStrLn "--- equivalence ---"
            printResult (TC.runM (TS.eqvtype tenv ty1 ty2) kenv TS.initCaches)
            exec tenv kenv cmds
          G.DSubst x e1 e2 ->
            print (show $ G.subst x e1 e2)
          G.DLub _ ty1 ty2 ->
            print (S.lub tenv ty1 ty2)
          G.DGlb _ ty1 ty2 ->
            print (S.glb tenv ty1 ty2)
          G.DSig x m ty ->
            exec ((x,(m, ty)) : tenv) kenv cmds
          G.DFun f binds e mty -> do
            putStrLn ("--- type checking: " ++ f ++ " ---")
            -- TODO: add f to env, but this requires its type
            let complete [] e = e
                complete ((m, v, vty) : binds) e = G.Lam m v vty (complete binds e)
                e' = complete binds e
                buildty ty [] = ty
                buildty ty ((m, v, vty) : binds) = G.TFun m v vty (buildty ty binds)
                    
            tenv' <- case mty of
              Nothing -> do
                let r = (TC.runM (TT.tySynth tenv e') kenv TS.initCaches)
                    tenv' = case fst r of
                          Left _ ->
                            tenv
                          Right ((ty, _), _) ->
                            ((f,(K.Many, buildty ty binds)) : tenv)
                printResult r
                return tenv'

              Just ty -> do
                let fty = buildty ty binds
                    tenv' = ((f,(K.Many, fty)) : tenv)
                printResult (TC.runM (TT.tyCheck tenv' e' fty) kenv TS.initCaches)
                return tenv'
            exec tenv' kenv cmds

          G.DType tid m k ty -> do
            putStrLn ("--- type declaration: " ++ tid ++ " ---")
            -- TODO: in general, we need to wait with this check until all types are declared
            let kenv' = (tid, (ty, k)):kenv
            -- print (Ty.kiCheck tenv ty k)
            printResult (TC.runM (TT.kiCheck tenv ty k) kenv' TS.initCaches)
            exec tenv kenv' cmds
          _ ->
            putStrLn "uninterpreted"
