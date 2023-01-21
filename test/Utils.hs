module Utils where

import Parsing
import Syntax
import Interpreter
import ProcessEnvironment
import Control.Monad.Reader (runReaderT)
import Test.Hspec
import Control.Concurrent.MVar

shouldParseDecl :: HasCallStack => String -> Decl -> Expectation
shouldParseDecl source expected =
  parseDecls source `shouldBe` Right [expected]

raiseFailure :: HasCallStack => String -> IO a
raiseFailure msg = do
  expectationFailure msg
  return $ error "expectationFailure did not abort"

shouldInterpretTo :: [Decl] -> Value -> Expectation
shouldInterpretTo givenDecls expectedValue = do
  mvar <- newEmptyMVar
  value <- runReaderT (interpretDecl givenDecls) ([], mvar)
  value `shouldBe` expectedValue

shouldThrowCastException :: [Decl] -> Expectation
shouldThrowCastException givenDecls =
  let isCastException :: InterpreterException -> Bool
      isCastException (CastException _) = True
      isCastException _ = False
  in do 
    mvar <- newEmptyMVar
    runReaderT (interpretDecl givenDecls) ([], mvar) `shouldThrow` isCastException

shouldThrowInterpreterException :: Decl -> InterpreterException -> Expectation
shouldThrowInterpreterException given except = do 
  mvar <- newEmptyMVar
  runReaderT (interpretDecl [given]) ([], mvar) `shouldThrow` (== except)

shouldInterpretTypeTo :: Type -> NFType -> Expectation
shouldInterpretTypeTo t expected = do
  mvar <- newEmptyMVar
  nft <- runReaderT (evalType t) ([], mvar)
  nft `shouldBe` expected
