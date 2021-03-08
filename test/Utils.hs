module Utils where

import Parsing
import Syntax
import Test.Hspec

shouldParseDecl :: HasCallStack => String -> Decl -> Expectation
shouldParseDecl source expected =
  parseDecls source `shouldBe` Right [expected]

raiseFailure :: HasCallStack => String -> IO a
raiseFailure msg = do
  expectationFailure msg
  return $ error "expectationFailure did not abort"
