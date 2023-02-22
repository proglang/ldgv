{-# LANGUAGE LambdaCase #-}
module ProcessEnvironmentTypes where
import Syntax as S
import Control.Concurrent.MVar as MVar
import Data.Set (Set)
import Data.Map as Map
import qualified Data.Set as Set
import Kinds (Multiplicity(..))

import qualified Networking.NetworkConnection as NCon

extendEnv :: String -> Value -> PEnv -> PEnv
extendEnv = curry (:)

-- | a Process Envronment maps identifiers to Values of expressions and stores
type PEnv = [PEnvEntry]
type PEnvEntry = (String, Value)

type Label = String
type LabelType = Set Label

labelsFromList :: [Label] -> LabelType
labelsFromList = Set.fromList

data FuncType = FuncType PEnv String S.Type S.Type
  deriving Eq

instance Show FuncType where
  show (FuncType _ s t1 t2) = "FuncType " ++ show s ++ " " ++ show t1 ++ " " ++ show t2

type ServerSocket = (MVar.MVar [(String, (Type, Type))], String)

type VChanConnections = MVar.MVar (Map.Map String (NCon.NetworkConnection Value))

type ValueRepr = String

-- | (Unit, Label, Int, Values of self-declared Data Types), Channels
data Value
  = VUnit
  | VLabel String
  | VInt Int
  | VDouble Double
  | VString String
  | VChan (NCon.NetworkConnection Value) (MVar.MVar Bool)
  | VChanSerial ([Value], Int, Int) ([Value], Int, Int) String String (String, String, String)
  | VSend Value
  | VPair Value Value -- pair of ids that map to two values
  | VType S.Type
  | VFunc PEnv String Exp
  | VDynCast Value GType -- (Value : G => *)
  | VFuncCast Value FuncType FuncType -- (Value : (ρ,α,Π(x:A)A') => (ρ,α,Π(x:B)B'))
  | VRec PEnv String String Exp Exp
  | VNewNatRec PEnv String String String Type Exp String Exp
  deriving Eq

modifyVChans ::  (Value -> IO Value) -> Value -> IO Value
modifyVChans vchanhandler input = case input of
    VSend v -> do
        nv <- modifyVChans vchanhandler v
        return $ VSend nv
    VPair v1 v2 -> do
        nv1 <- modifyVChans vchanhandler v1
        nv2 <- modifyVChans vchanhandler v2
        return $ VPair nv1 nv2
    VFunc penv a b -> do
        newpenv <- modifyVChansPEnv vchanhandler penv
        return $ VFunc newpenv a b
    VDynCast v g -> do
        nv <- modifyVChans vchanhandler v
        return $ VDynCast nv g
    VFuncCast v a b -> do
        nv <- modifyVChans vchanhandler v
        return $ VFuncCast nv a b
    VRec penv a b c d -> do
        newpenv <- modifyVChansPEnv vchanhandler penv
        return $ VRec newpenv a b c d
    VNewNatRec penv a b c d e f g -> do
        newpenv <- modifyVChansPEnv vchanhandler penv
        return $ VNewNatRec newpenv a b c d e f g
    VChan nc used-> vchanhandler input
    VChanSerial r w p o c -> vchanhandler input
    _ -> return input
    where
        modifyVChansPEnv :: (Value -> IO Value) ->  [(String, Value)] -> IO [(String, Value)]
        modifyVChansPEnv _ [] = return []
        modifyVChansPEnv vchanhandler  penvs@(x:xs) = do
            newval <- modifyVChans vchanhandler $ snd x
            rest <- modifyVChansPEnv vchanhandler xs
            return $ (fst x, newval):rest

searchVChans ::  (Value -> IO r) -> r -> (r -> r -> r) -> Value -> IO r
searchVChans vchanhandler defaultResult mergeResults input = case input of
    VSend v -> searchVChans vchanhandler defaultResult mergeResults v
        
    VPair v1 v2 -> do
        nv1 <- searchVChans vchanhandler defaultResult mergeResults v1
        nv2 <- searchVChans vchanhandler defaultResult mergeResults v2
        return $ mergeResults nv1 nv2
    VFunc penv a b -> searchVChansPEnv vchanhandler defaultResult mergeResults penv
    VDynCast v g -> searchVChans vchanhandler defaultResult mergeResults v
    VFuncCast v a b -> searchVChans vchanhandler defaultResult mergeResults v
    VRec penv a b c d -> searchVChansPEnv vchanhandler defaultResult mergeResults penv
    VNewNatRec penv a b c d e f g -> searchVChansPEnv vchanhandler defaultResult mergeResults penv
    VChan nc used-> vchanhandler input
    VChanSerial r w p o c -> vchanhandler input
    _ -> return defaultResult
    where
        searchVChansPEnv :: (Value -> IO r) -> r -> (r -> r -> r) -> [(String, Value)] -> IO r
        searchVChansPEnv _ defaultResult _ [] = return defaultResult
        searchVChansPEnv vchanhandler defaultResult mergeResults penvs@(x:xs) = do
            newval <- searchVChans vchanhandler defaultResult mergeResults $ snd x
            rest <- searchVChansPEnv vchanhandler defaultResult mergeResults xs
            return $ mergeResults newval rest

disableOldVChan :: Value -> IO Value
disableOldVChan value = case value of
  VChan nc used -> do
      _ <- MVar.takeMVar used
      MVar.putMVar used True
      unused <- MVar.newEmptyMVar
      MVar.putMVar unused False
      return $ VChan nc unused
  _ -> return value

instance Show Value where
  show = \case
    VUnit -> "VUnit"
    VLabel s -> "VLabel " ++ s
    VInt i -> "VInt " ++ show i
    VDouble d -> "VDouble " ++ show d
    VString s -> "VString \"" ++ show s ++ "\""
    VChan {} -> "VChan"
    VChanSerial {} -> "VChanSerial"
    VSend v -> "VSend (" ++ show v ++ ")"
    VPair a b -> "VPair <" ++ show a ++ ", " ++ show b ++ ">"
    VType t -> "VType " ++ show t
    VFunc _ s exp -> "VFunc " ++ show s ++ " " ++ show exp
    VDynCast v t -> "VDynCast (" ++ show v ++ ") (" ++ show t ++ ")"
    VFuncCast v ft1 ft2 -> "VFuncCast (" ++ show v ++ ") (" ++ show ft1 ++ ") (" ++ show ft2 ++ ")"
    VRec env f x e1 e0 -> "VRec " ++ " " ++ f ++ " " ++ x ++ " " ++ show e1 ++ " " ++ show e0
    VNewNatRec env f n tid ty ez x es -> "VNewNatRec " ++ f ++ n ++ tid ++ show ty ++ show ez ++ x ++ show es

class Subtypeable t where
  isSubtypeOf :: t -> t -> Bool

-- Types in Head Normal Form
data NFType
  = NFBot
  | NFDyn
  | NFFunc FuncType  -- (ρ, α, Π(x: A) B)
  | NFPair FuncType  -- (ρ, α, Σ(x: A) B)
  | NFGType GType -- every ground type is also a type in normal form
  deriving (Show, Eq)

instance Subtypeable NFType where
  -- NFFunc and NFPair default to false, which is not really correct.
  -- Implementation would be quite complicated and its not necessary,
  -- i.e. not used anywhere.
  isSubtypeOf NFBot _ = True
  isSubtypeOf NFDyn NFDyn = True
  isSubtypeOf (NFGType gt1) (NFGType gt2) = gt1 `isSubtypeOf` gt2
  isSubtypeOf _ _ = False

data GType
  = GUnit
  | GLabel LabelType
  | GFunc Multiplicity -- Π(x: *) *
  | GPair -- Σ(x: *) *
  | GNat
  | GNatLeq Integer
  | GInt
  | GDouble
  | GString
  deriving (Show, Eq)

instance Subtypeable GType where
  isSubtypeOf GUnit GUnit = True
  isSubtypeOf (GLabel ls1) (GLabel ls2) = ls1 `Set.isSubsetOf` ls2
  isSubtypeOf (GFunc _) (GFunc _) = True
  isSubtypeOf (GPair) (GPair) = True
  isSubtypeOf GNat GNat = True
  isSubtypeOf (GNatLeq _) GNat = True
  isSubtypeOf (GNatLeq n1) (GNatLeq n2) = n1 <= n2
  isSubtypeOf GInt GInt = True
  isSubtypeOf GDouble GDouble = True
  isSubtypeOf GString GString = True
  isSubtypeOf _ _ = False
