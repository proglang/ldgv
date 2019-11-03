module ProcessEnvironment where
import Syntax as S
import qualified TCSubtyping as TS
import Control.Concurrent.Chan as C
import Control.Monad.State as T
import Control.Monad

-- | the interpretation monad
type InterpretM = T.StateT PEnv IO Value

-- | create a new entry (requires identifier to be unique)
createPMEntry :: PEnvEntry -> T.StateT PEnv IO ()
createPMEntry entry = do
    liftIO $ putStrLn $ "Creating Environment entry " ++ show entry
    modify (\s1 -> entry : s1)

extendEnv :: PEnvEntry -> PEnv -> PEnv
extendEnv e env = e:env

-- | Ignore current environment end perform a computation in a given environment
-- inEnv :: PEnv -> InterpretM -> InterpretM
-- inEnv env ma = \_ -> env >>= ma

pmlookup :: String -> InterpretM
pmlookup id = do
    identifiers <- get
    case lookup id identifiers of
        Nothing -> fail ("No Value for identifier " ++ id ++ " in ProcessEnvironment")
        Just val -> do
                    liftIO $ putStrLn $ "Looked up " ++ id ++ " and found " ++ show val
                    >> pure val

-- | a Process Envronment maps identifiers to Values of expressions and stores
type PEnv = [PEnvEntry]
type PEnvEntry = (String, Value)

argsfromDFun :: S.Decl -> [(String, S.Type)]
argsfromDFun (DFun s args e mt) = [(s, t) | (m, s, t) <- args]


lookupEnv :: String -> PEnv -> Value
lookupEnv s penv = case lookup s penv of
                    Just val -> val

printPEnv :: PEnv -> String
printPEnv [] = ""
printPEnv (x:xs) = show x ++ "\n" ++ printPEnv xs

-- | (Unit, Label, Int, Values of self-declared Data Types), Channels
data Value = VUnit
      | VLabel String
      | VInt Int
      | VChan (C.Chan Value) -- when an id maps to a Channel
      | VPair Value Value -- pair of ids that map to two values
      | VDecl S.Decl -- when an identifier maps to another function
      | VType S.Type
      -- | represents an unfinished send on a channel.
      -- | Holds a function that expects a Value to be sent on the channel
      | VFun (Value -> InterpretM) -- Function Type


instance Show Value where
    show v = case v of
        VUnit -> "VUnit"
        VLabel s -> "VLabel " ++ s
        VInt i -> "VInt " ++ show i
        VChan _ -> "VChan"
        VPair a b -> "(" ++ show a ++ "," ++ show b ++ ")"
        VDecl d -> "VDecl " ++ show d
        VType t -> "VType " ++ show t
        VFun _ -> "VFunction "
        _ -> "Value"

