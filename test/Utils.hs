module Utils where

import Parsing
import Syntax
import Interpreter
import ProcessEnvironment
import Control.Monad.Reader (runReaderT)
import Test.Hspec

shouldParseDecl :: HasCallStack => String -> Decl -> Expectation
shouldParseDecl source expected =
  parseDecls source `shouldBe` Right [expected]

raiseFailure :: HasCallStack => String -> IO a
raiseFailure msg = do
  expectationFailure msg
  return $ error "expectationFailure did not abort"

shouldInterpretTo :: [Decl] -> Value -> Expectation
shouldInterpretTo givenDecls expectedValue = do
  value <- runReaderT (interpretDecl givenDecls) []
  value `shouldBe` expectedValue

shouldThrowCastException :: [Decl] -> Expectation
shouldThrowCastException givenDecls =
  let isCastException :: InterpreterException -> Bool
      isCastException (CastException _) = True
      isCastException _ = False
  in runReaderT (interpretDecl givenDecls) [] `shouldThrow` isCastException

shouldThrowInterpreterException :: Decl -> InterpreterException -> Expectation
shouldThrowInterpreterException given except = runReaderT (interpretDecl [given]) [] `shouldThrow` (== except)

shouldInterpretTypeTo :: Type -> NFType -> Expectation
shouldInterpretTypeTo t expected = do
  nft <- runReaderT (evalType t) []
  nft `shouldBe` expected
