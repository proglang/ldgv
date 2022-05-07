{-# LANGUAGE LambdaCase #-}

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

shouldInterpretInPEnvTo :: PEnv -> Decl -> Value -> Expectation
shouldInterpretInPEnvTo penv givenDecl expectedValue = do
  value <- runReaderT (evalDFun givenDecl) penv
  value `shouldBe` expectedValue

shouldThrowCastException :: PEnv -> Decl -> Expectation
shouldThrowCastException penv givenDecl =
  let isCastException :: InterpreterException -> Bool
      isCastException (CastException _) = True
      isCastException _ = False
  in runReaderT (evalDFun givenDecl) penv `shouldThrow` isCastException

shouldThrowInterpreterException :: Decl -> InterpreterException -> Expectation
shouldThrowInterpreterException given except = runReaderT (evalDFun given) [] `shouldThrow` (== except)

shouldInterpretTo :: Decl -> Value -> Expectation
shouldInterpretTo = shouldInterpretInPEnvTo []

shouldInterpretTypeTo :: Type -> NFType -> Expectation
shouldInterpretTypeTo t expected = do
  nft <- runReaderT (evalType t) []
  nft `shouldBe` expected
