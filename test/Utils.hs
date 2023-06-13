module Utils where

import Parsing
import Syntax
import Interpreter
import ProcessEnvironmentTypes
import qualified Networking.Common as NC
import qualified Networking.Serialize as NSer
import qualified ValueParsing.ValueGrammar as VG
import qualified ValueParsing.ValueTokens as VT
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

serializesConsistentlyValue :: NSer.Serializable a => a -> Expectation
serializesConsistentlyValue serializable = 
  let serial = NSer.serialize serializable in
  case VT.runAlex serial VG.parseValues of
        Left err ->  err `shouldBe` serial
        Right deserial -> NSer.serialize deserial `shouldBe` serial

serializesConsistentlyMessage :: NSer.Serializable a => a -> Expectation
serializesConsistentlyMessage serializable = 
  let serial = NSer.serialize serializable in
  case VT.runAlex serial VG.parseMessages of
        Left err ->  err `shouldBe` serial
        Right deserial -> NSer.serialize deserial `shouldBe` serial

serializesConsistentlyResponse :: NSer.Serializable a => a -> Expectation
serializesConsistentlyResponse serializable = 
  let serial = NSer.serialize serializable in
  case VT.runAlex serial VG.parseResponses of
        Left err ->  err `shouldBe` serial
        Right deserial -> NSer.serialize deserial `shouldBe` serial

serializesConsistentlyConversation :: NSer.Serializable a => a -> Expectation
serializesConsistentlyConversation serializable = 
  let serial = NSer.serialize serializable in
  case VT.runAlex serial VG.parseConversation of
        Left err ->  err `shouldBe` serial
        Right deserial -> NSer.serialize deserial `shouldBe` serial