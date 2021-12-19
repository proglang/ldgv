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

data FuncType = FuncType PEnv String S.Type S.Type
  deriving (Show, Eq)

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
  | VDynCast Value NFType -- (Value : G => *)
  | VFuncCast Value FuncType FuncType -- (Value : (ρ,Π(x:A)A') => (ρ,Π(x:B)B'))

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
    VFun _ -> "VFunction"
    VDynCast v t -> "VDynCast (" ++ show v ++ " : " ++ show t ++ "=> *)"
    VFuncCast v ft1 ft2 -> "VFuncCast (" ++ show v ++ " : " ++ show ft1 ++ " => " ++ show ft2

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
  | NFLabel (Set String)
  | NFSingleton Value NFType          -- S{V:A}
  | NFFunc FuncType  -- (ρ, Π(x: A) B)
  | NFPair FuncType  -- (ρ, Σ(x: A) B)
  | NFInt
  | NFDouble
  deriving Show

instance Eq NFType where
  NFBot == NFBot = True
  NFDyn == NFDyn = True
  NFUnit == NFUnit = True
  NFLabel ls1 == NFLabel ls2 = ls1 == ls2
  (NFFunc ft1) == (NFFunc ft2) = ft1 == ft2
  (NFPair ft1) == (NFPair ft2) = ft1 == ft2
  NFInt == NFInt = True
  NFDouble == NFDouble = True
  _ == _ = False
