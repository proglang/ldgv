module Utils where

import Parsing
import Syntax
import Interpreter
import ProcessEnvironment
import Control.Monad.State (evalStateT)
import Test.Hspec

shouldParseDecl :: HasCallStack => String -> Decl -> Expectation
shouldParseDecl source expected =
  parseDecls source `shouldBe` Right [expected]

raiseFailure :: HasCallStack => String -> IO a
raiseFailure msg = do
  expectationFailure msg
  return $ error "expectationFailure did not abort"

shouldInterpretTo :: Decl -> Value -> Expectation
shouldInterpretTo givenDecl expectedValue = do
  value <- evalStateT (evalDFun givenDecl) []
  (return value) `shouldReturn` expectedValue
