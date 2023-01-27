{-# LANGUAGE LambdaCase #-}
module ProcessEnvironmentTypes where
import Syntax as S
import GHC.IO.Handle
import Control.Concurrent.Chan as C
import Control.Concurrent.MVar as MVar
import Control.Monad.Reader as T
import Data.Set (Set)
import Data.Map as Map
import qualified Data.Set as Set
import Kinds (Multiplicity(..))

import qualified Data.Maybe

import Networking.DirectionalConnection
import qualified Networking.NetworkConnection as NCon
-- import qualified Networking.Common as NC

import Network.Socket

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

-- data NetworkAddress = NetworkAddress {hostname :: String, port :: String}
--   deriving (Eq, Show)

type ServerSocket = (MVar.MVar (Map.Map String (NCon.NetworkConnection Value)), MVar.MVar [(String, Type)], String)

type ValueRepr = String

-- | (Unit, Label, Int, Values of self-declared Data Types), Channels
data Value
  = VUnit
  | VLabel String
  | VInt Int
  | VDouble Double
  | VString String
  | VChan (NCon.NetworkConnection Value) (MVar.MVar (Map.Map String (NCon.NetworkConnection Value))) (MVar.MVar Bool) --Maybe a "used" mvar to notify that this vchan should no longer be used
  --                                                This is exclusively used to add VChanSerials into the map when in the interpreter
  --                                                                                                    This is to mark a vchan as used (true if used)
  | VChanSerial ([Value], Int) ([Value], Int) String String (String, String)
  | VSend Value
  | VPair Value Value -- pair of ids that map to two values
  | VType S.Type
  | VFunc PEnv String Exp
  | VDynCast Value GType -- (Value : G => *)
  | VFuncCast Value FuncType FuncType -- (Value : (ρ,α,Π(x:A)A') => (ρ,α,Π(x:B)B'))
  | VRec PEnv String String Exp Exp
  | VNewNatRec PEnv String String String Type Exp String Exp
  | VServerSocket (MVar.MVar (Map.Map String (NCon.NetworkConnection Value))) (MVar.MVar [(String, Type)]) String
                                                                                              -- Own Port Number
  deriving Eq

disableOldVChan :: Value -> IO Value
disableOldVChan value = case value of
  VChan nc mvar used -> do
      _ <- MVar.takeMVar used
      MVar.putMVar used True
      unused <- MVar.newEmptyMVar
      MVar.putMVar unused False
      return $ VChan nc mvar unused
  _ -> return value


disableVChan :: Value -> IO ()
disableVChan value = case value of
  VChan nc mvar _ -> do
    mbystate <- MVar.tryTakeMVar $ NCon.ncConnectionState nc  --I dont fully understand why this mvar isnt filled but lets bypass this problem
    case mbystate of
      Nothing -> MVar.putMVar (NCon.ncConnectionState nc) NCon.Disconnected
      Just state -> case state of
          NCon.Connected _ _ -> MVar.putMVar (NCon.ncConnectionState nc) NCon.Disconnected
          NCon.Emulated -> MVar.putMVar (NCon.ncConnectionState nc) NCon.Disconnected
          _ -> MVar.putMVar (NCon.ncConnectionState nc) state
  _ -> return ()



disableVChans :: Value -> IO ()
disableVChans input = case input of
    VSend v -> do
        nv <- disableVChans v
        return ()
        -- return $ VSend nv
    VPair v1 v2 -> do 
        nv1 <- disableVChans v1
        nv2 <- disableVChans v2
        return ()
        -- return $ VPair nv1 nv2
    VFunc penv a b -> do
        newpenv <- disableVChansPEnv penv
        return ()
        -- return $ VFunc newpenv a b
    VDynCast v g -> do 
        nv <- disableVChans v
        return ()
        -- return $ VDynCast nv g
    VFuncCast v a b -> do 
        nv <- disableVChans v
        return ()
        -- return $ VFuncCast nv a b
    VRec penv a b c d -> do 
        newpenv <- disableVChansPEnv penv
        return ()
        -- return $ VRec newpenv a b c d
    VNewNatRec penv a b c d e f g -> do 
        newpenv <- disableVChansPEnv penv
        return ()
        -- return $ VNewNatRec newpenv a b c d e f g
    _ -> disableVChan input -- This handles vchans and the default case
    where
        disableVChansPEnv :: [(String, Value)] -> IO ()
        disableVChansPEnv [] = return ()
        disableVChansPEnv (x:xs) = do 
            newval <- disableVChans $ snd x
            rest <- disableVChansPEnv xs
            return ()
            -- return $ (fst x, newval):rest


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
    VServerSocket {} -> "VServerSocket"

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
