module Utils where

import Parsing
import Syntax
import Interpreter
import ProcessEnvironment
import ProcessEnvironmentTypes
-- import qualified Networking.NetworkingMethod.NetworkingMethodCommon as NMC
-- import qualified Networking.NetworkingMethod.Stateless as Stateless
import qualified Networking.Common as NC
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
  sockets <- newMVar Map.empty
  vchanconnections <- newMVar Map.empty
  handles <- NC.createActiveConnections
  value <- runReaderT (interpretDecl givenDecls) ([], (sockets, vchanconnections, handles))
  value `shouldBe` expectedValue

shouldThrowCastException :: [Decl] -> Expectation
shouldThrowCastException givenDecls =
  let isCastException :: InterpreterException -> Bool
      isCastException (CastException _) = True
      isCastException _ = False
  in do 
    sockets <- newMVar Map.empty
    vchanconnections <- newMVar Map.empty
    handles <- NC.createActiveConnections
    runReaderT (interpretDecl givenDecls) ([], (sockets, vchanconnections, handles)) `shouldThrow` isCastException

shouldThrowInterpreterException :: Decl -> InterpreterException -> Expectation
shouldThrowInterpreterException given except = do 
  sockets <- newMVar Map.empty
  vchanconnections <- newMVar Map.empty
  handles <- NC.createActiveConnections
  runReaderT (interpretDecl [given]) ([], (sockets, vchanconnections, handles)) `shouldThrow` (== except)

shouldInterpretTypeTo :: Type -> NFType -> Expectation
shouldInterpretTypeTo t expected = do
  sockets <- newMVar Map.empty
  vchanconnections <- newMVar Map.empty
  handles <- NC.createActiveConnections
  nft <- runReaderT (evalType t) ([], (sockets, vchanconnections, handles))
  nft `shouldBe` expected
