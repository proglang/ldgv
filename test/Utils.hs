module Utils where

import Parsing
import Syntax
import Interpreter
import ProcessEnvironment
import Control.Monad.State.Strict (evalStateT)
import Test.Hspec

shouldParseDecl :: HasCallStack => String -> Decl -> Expectation
shouldParseDecl source expected =
  parseDecls source `shouldBe` Right [expected]

raiseFailure :: HasCallStack => String -> IO a
raiseFailure msg = do
  expectationFailure msg
  return $ error "expectationFailure did not abort"

shouldInterpretInPEnvTo :: [Decl] -> Decl -> Value -> Expectation
shouldInterpretInPEnvTo decls givenDecl expectedValue = do
  let penv = createPEnv decls
  value <- evalStateT (evalDFun givenDecl) penv
  return value `shouldReturn` expectedValue

shouldThrowCastException :: [Decl] -> Decl -> Expectation
shouldThrowCastException decls givenDecl =
  let penv = createPEnv decls
      isCastException :: InterpreterException -> Bool
      isCastException (CastException _) = True
      isCastException _ =False in
    evalStateT (evalDFun givenDecl) penv `shouldThrow` isCastException

shouldInterpretTo :: Decl -> Value -> Expectation
shouldInterpretTo = shouldInterpretInPEnvTo []
