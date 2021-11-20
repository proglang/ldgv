module Utils where

import Parsing
import Syntax
import Kinds
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

-- type Bool : ~un = {'T, 'F}
boolTypeDecl :: Decl
boolTypeDecl = DType "Bool" MMany Kun (TLab ["'T","'F"])

-- val not(b: Bool) = (case b {'T: 'F, 'F: 'T})
notFuncDecl :: Decl
notFuncDecl = DFun
    "not" [(MMany,"b",TName False "Bool")]
    (Case (Var "b") [("'T",Lit (LLab "'F")),("'F",Lit (LLab "'T"))])
    Nothing

shouldInterpretTo :: Decl -> Value -> Expectation
shouldInterpretTo givenDecl expectedValue = do
  -- load some functions in the environment
  let penv = createPEnv [boolTypeDecl, notFuncDecl]
  value <- evalStateT (evalDFun givenDecl) penv
  return value `shouldReturn` expectedValue
