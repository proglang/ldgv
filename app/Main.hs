module Main where

import qualified Tokens as T
import qualified Grammar as G
import qualified Syntax as G
import qualified Subtyping as S
import qualified Typing as Ty
import qualified Kinds as K
import qualified Interpreter as I
import qualified TCXMonad as TC
import qualified TCSubtyping as TS
import qualified TCTyping as TT
import Options.Applicative
import Data.Semigroup ((<>))
import Config as C


-- | Options for the main program
data LDGVOptions = LDGVOptions
  { interpret :: Bool
--  , parseonly :: Bool
  , file      :: String }

-- | Options descriptions, types and defaults
processOptions :: Parser LDGVOptions
processOptions = LDGVOptions
  <$> switch
    (long "interpret"
    <> short 'i'
    <> help "interpret (instead of typecheck) the value of a \"main\" function definition (with no free variables)")
  <*> argument str ( metavar "FILENAME")

-- | execute the option parser or help message printer
main :: IO ()
main = execOptions =<< execParser opts
  where
    opts = info (processOptions <**> helper)
      ( fullDesc
     <> progDesc "A typechecker / interpreter for ldgv files")

-- | typecheck or interpret the file according to the specified args
execOptions :: LDGVOptions -> IO ()
execOptions o@(LDGVOptions True filename) = do
                                                typecheck filename o
                                                putStrLn "-------- Typecheck OK, interpreting --------"
                                                I.interpret filename
execOptions o@(LDGVOptions False filename) = typecheck filename o

-- | typecheck a given ldgv file
typecheck :: String -> LDGVOptions -> IO ()
typecheck filename _ = do
  s <- readFile filename
  let ts = T.alexScanTokens s
  let cmds = G.parseCalc ts
  C.printDebug ts
  C.printDebug cmds
  putStrLn "-------- running typecheck request --------"
  exec [] [] cmds
    where
      exec tenv kenv [] = return ()
      exec tenv kenv (cmd:cmds) = 
        case cmd of
          G.DSub ty1 ty2 -> do
            C.putDebugStr "--- subtyping ---"
            C.printResult (TC.runM (TS.subtype tenv ty1 ty2) kenv TS.initCaches)
            exec tenv kenv cmds
            -- C.printDebug (S.subtype tenv ty1 ty2)
          G.DEqv ty1 ty2 -> do
            C.putDebugStr "--- equivalence ---"
            C.printResult (TC.runM (TS.eqvtype tenv ty1 ty2) kenv TS.initCaches)
            exec tenv kenv cmds
          G.DSubst x e1 e2 ->
            C.printDebug (show $ G.subst x e1 e2)
          G.DLub _ ty1 ty2 ->
            C.printDebug (S.lub tenv ty1 ty2)
          G.DGlb _ ty1 ty2 ->
            C.printDebug (S.glb tenv ty1 ty2)
          G.DSig x m ty ->
            exec ((x,(m, ty)) : tenv) kenv cmds
          G.DFun f binds e mty -> do
            C.putDebugStr ("--- type checking: " ++ f ++ " ---")
            -- TODO: add f to env, but this requires its type
            let complete [] e = e
                complete ((m, v, vty) : binds) e = G.Lam m v vty (complete binds e)
                e' = complete binds e
                buildty ty [] = ty
                buildty ty ((m, v, vty) : binds) = G.TFun m v vty (buildty ty binds)
                    
            tenv' <- case mty of
              Nothing -> do
                let r = (TC.runM (TT.tySynthUnfold tenv e') kenv TS.initCaches)
                    tenv' = case fst r of
                          Left _ ->
                            tenv
                          Right ((ty, _), _) ->
                            ((f,(K.Many, ty)) : tenv)
                C.printResult r
                return tenv'

              Just ty -> do
                let fty = buildty ty binds
                    tenv' = ((f,(K.Many, fty)) : tenv)
                C.printResult (TC.runM (TT.tyCheck tenv' e' fty) kenv TS.initCaches)
                return tenv'
            exec tenv' kenv cmds

          G.DType tid m k ty -> do
            C.putDebugStr ("--- type declaration: " ++ tid ++ " ---")
            -- TODO: in general, we need to wait with this check until all types are declared
            let kenv' = (tid, (ty, k)):kenv
            -- C.printDebug (Ty.kiCheck tenv ty k)
            C.printResult (TC.runM (TT.kiCheck tenv ty k) kenv' TS.initCaches)
            exec tenv kenv' cmds
          _ ->
            C.putDebugStr "uninterpreted"
