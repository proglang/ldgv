{-# LANGUAGE LambdaCase #-}

module ProcessEnvironment where
import Syntax as S
import qualified Config as D
import Control.Concurrent.Chan as C
import Control.Monad.State.Strict as T
import Data.Set (Set)
import qualified Data.Set as Set

-- | the interpretation monad
type InterpretM a = T.StateT PEnv IO a

-- | create a new entry (requires identifier to be unique)
createPMEntry :: PEnvEntry -> T.StateT PEnv IO ()
createPMEntry entry = do
  liftIO $ D.traceIO $ "Creating Environment entry " ++ show entry
  modify (entry :)

extendEnv :: PEnvEntry -> PEnv -> PEnv
extendEnv e env = e:env

pmlookup :: String -> InterpretM Value
pmlookup id = do
  identifiers <- get
  case lookup id identifiers of
    Nothing -> fail ("No Value for identifier " ++ id ++ " in ProcessEnvironment")
    Just val -> do
      liftIO $ D.traceIO $ "Looked up " ++ id ++ " and found " ++ show val
      >> pure val

-- | a Process Envronment maps identifiers to Values of expressions and stores
type PEnv = [PEnvEntry]
type PEnvEntry = (String, Value)

printPEnv :: PEnv -> String
printPEnv = foldr (\entry string -> show entry ++ "\n" ++ string) ""

type Label = String
type LabelType = Set Label

-- Ground Type
data GType = GUnit
  | GLabel LabelType
  | GUnitSingleton                  -- S{():Unit)
  | GLabelSingleton Value LabelType -- S{V:L}
  | GSum                            -- Π(x: *)* aka * → *
  | GProd                           -- Σ(x: *)* aka * ⨯ *

instance Show GType where
  show = \case
    GUnit -> "GUnit"
    GLabel ls -> "GLabel " ++ show ls
    GUnitSingleton -> "GUnitSingleton {():Unit}"
    GLabelSingleton v ls -> "GLabelSingleton {" ++ show v ++ ": " ++ show ls ++ "}"
    GSum -> "GSum"
    GProd -> "GProd"

instance Eq GType where
  GUnit == GUnit = True
  (GLabel ls1) == (GLabel ls2) = ls1 == ls2
  GUnitSingleton == GUnitSingleton = True
  (GLabelSingleton v1 ls1) == (GLabelSingleton v2 ls2) = v1 == v2 && ls1 == ls2
  GSum == GSum = True
  GProd == GProd = True
  _ == _ = False

-- | (Unit, Label, Int, Values of self-declared Data Types), Channels
data Value = VUnit
  | VLabel String
  | VInt Int
  | VDouble Double
  -- we have two channels, one for reading and one for writing to the other
  -- end, so we do not read our own written values
  | VChan (C.Chan Value) (C.Chan Value)
  | VPair Value Value -- pair of ids that map to two values
  | VDecl S.Decl -- when an identifier maps to another function we have not yet interpreted
  | VType S.Type
  | VFun (Value -> InterpretM Value) -- Function Type
  | VDynCast Value GType -- (Value : G => *)

instance Show Value where
  show = \case
    VUnit -> "VUnit"
    VLabel s -> "VLabel " ++ s
    VInt i -> "VInt " ++ show i
    VDouble d -> "VDouble " ++ show d
    VChan _ _ -> "VChan"
    VPair a b -> "(" ++ show a ++ "," ++ show b ++ ")"
    VDecl d -> "VDecl " ++ show d
    VType t -> "VType " ++ show t
    VFun _ -> "VFunction "
    VDynCast v t -> "VDynCast (" ++ show v ++ ":" ++ show t ++ "=> *)"

instance Eq Value where
  VUnit == VUnit = True
  (VLabel s1) == (VLabel s2) = s1 == s2
  (VInt i1) == (VInt i2) = i1 == i2
  (VDouble d1) == (VDouble d2) = d1 == d2
  (VChan r1 w1) == (VChan r2 w2) = r1 == r2 && w1 == w2
  (VPair x1 y1) == (VPair x2 y2) = x1 == x2 && y1 == y2
  (VDecl d1) == (VDecl d2) = d1 == d2
  (VType t1) == (VType t2) = t1 == t2
  (VDynCast v1 t1) == (VDynCast v2 t2) = v1 == v2 && t1 == t2
  _ == _ = False

-- Types in Head Normal Form
data NFType = NFBot
  | NFDyn
  | NFUnit
  | NFLab (Set String)
  | NFSingleton Value NFType          -- S{V:A}
  | NFApply PEnv String S.Type S.Type -- (ρ, Θ(x: A) B)
  | NFInt
  | NFDouble

instance Show NFType where
  show = \case
    NFBot -> "NFBot"
    NFDyn -> "NFDyn"
    NFUnit -> "NFUnit"
    NFLab labels -> "NFLab {" ++ foldr (\la lb -> la ++ "," ++ lb) "" labels ++ "}"
    NFSingleton v t -> "NFSingleton {" ++ show v ++ ": " ++ show t ++ "}"
    NFApply penv v tA tB -> "NFApply (" ++ show penv ++ ", Θ(" ++ show v ++ ":" ++ show tA ++ ") " ++ show tB ++ ")"
    NFInt -> "NFInt"
    NFDouble -> "NFDouble"

instance Eq NFType where
  NFBot == NFBot = True
  NFDyn == NFDyn = True
  NFUnit == NFUnit = True
  NFLab ls1 == NFLab ls2 = ls1 == ls2
  (NFApply penv1 var1 typeA1 typeB1) == (NFApply penv2 var2 typeA2 typeB2) =
    penv1 == penv2 && var1 == var2 && typeA1 == typeA2 && typeB1 == typeB2
  NFInt == NFInt = True
  NFDouble == NFDouble = True
  _ == _ = False
