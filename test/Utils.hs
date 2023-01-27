module Utils where

import Parsing
import Syntax
import Interpreter
import ProcessEnvironment
import ProcessEnvironmentTypes
import qualified Networking.NetworkingMethod.NetworkingMethodCommon as NMC
import qualified Networking.NetworkingMethod.Stateless as Stateless
import Control.Monad.Reader (runReaderT)
import Test.Hspec
import Control.Concurrent.MVar
import qualified Data.Map as Map

shouldParseDecl :: HasCallStack => String -> Decl -> Expectation
shouldParseDecl source expected =
  parseDecls source `shouldBe` Right [expected]

raiseFailure :: HasCallStack => String -> IO a
raiseFailure msg = do
  expectationFailure msg
  return $ error "expectationFailure did not abort"

shouldInterpretTo :: [Decl] -> Value -> Expectation
shouldInterpretTo givenDecls expectedValue = do
  sockets <- newEmptyMVar
  handles <- Stateless.createActiveConnections
  putMVar sockets Map.empty
  value <- runReaderT (interpretDecl givenDecls) ([], (sockets, handles))
  value `shouldBe` expectedValue

shouldThrowCastException :: [Decl] -> Expectation
shouldThrowCastException givenDecls =
  let isCastException :: InterpreterException -> Bool
      isCastException (CastException _) = True
      isCastException _ = False
  in do 
    sockets <- newEmptyMVar
    handles <- Stateless.createActiveConnections
    putMVar sockets Map.empty
    runReaderT (interpretDecl givenDecls) ([], (sockets, handles)) `shouldThrow` isCastException

shouldThrowInterpreterException :: Decl -> InterpreterException -> Expectation
shouldThrowInterpreterException given except = do 
  sockets <- newEmptyMVar
  handles <- Stateless.createActiveConnections
  putMVar sockets Map.empty
  runReaderT (interpretDecl [given]) ([], (sockets, handles)) `shouldThrow` (== except)

shouldInterpretTypeTo :: Type -> NFType -> Expectation
shouldInterpretTypeTo t expected = do
  sockets <- newEmptyMVar
  handles <- Stateless.createActiveConnections
  putMVar sockets Map.empty
  nft <- runReaderT (evalType t) ([], (sockets, handles))
  nft `shouldBe` expected
