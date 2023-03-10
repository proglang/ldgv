module SerializeSpec (spec) where

import Networking.Messages
import Syntax
import Test.Hspec
import ProcessEnvironmentTypes
import qualified Data.Set as Set

import Utils

spec :: Spec
spec = do
    describe "Serialize Values" $ do 
        it "VUnit" $ serializesConsistentlyValue VUnit
        it "VLabel" $ serializesConsistentlyValue $ VLabel "MyLabel"
        it "VInt" $ serializesConsistentlyValue $ VInt 42
        it "VDouble" $ serializesConsistentlyValue $ VDouble 42.1337
        it "VString" $ serializesConsistentlyValue $ VString "Hello World"
        it "VSend" $ serializesConsistentlyValue $ VSend $ VString "Hello World"
        it "VPair" $ serializesConsistentlyValue $ VPair (VInt 1337) (VLabel "var")
        it "VType" $ serializesConsistentlyValue $ VType $ TNatLeq 42
        it "VFunc" $ serializesConsistentlyValue $ VFunc [("s1", VString "String1"), ("i1", VInt 32)] "VFuncString" (Math (Add (Var "v1") (Lit (LInt 42))))
        it "VDynCast" $ serializesConsistentlyValue $ VDynCast VUnit (GLabel (Set.fromList ["String1", "String2"]))
        it "VFuncCast" $ serializesConsistentlyValue $ VFuncCast (VInt 42) (FuncType [("s1", VString "String1"), ("i1", VInt 32)] "FuncTypeString" TUnit TInt) (FuncType [("s2", VString "String2"), ("i2", VInt 42)] "FuncTypeString2" TInt (TVar False "Ident1"))
        it "VChanSerial" $ serializesConsistentlyValue $ VChanSerial ([VInt 42], 2, 3) ([], 0, 0) "partnerID" "ownID" ("127.0.0.1", "4242", "conversationID")
    describe "Serialize Messages" $ do
        it "Introduce" $ serializesConsistentlyMessage $ Introduce "userID" "4242" (TName False "TestName") (TSend "#!" TInt (TRecv "#?" TString TUnit))
        it "NewValue" $ serializesConsistentlyMessage $ NewValue "userID" 2 $ VInt 42
        it "RequestValue" $ serializesConsistentlyMessage $ RequestValue "userID" 42
        it "AcknowledgeValue" $ serializesConsistentlyMessage $ AcknowledgeValue "userID" 42
        it "NewPartnerAddress" $ serializesConsistentlyMessage $ NewPartnerAddress "userID" "4200" "conID1337"
        it "AcknowledgePartnerAddress" $ serializesConsistentlyMessage $ AcknowledgePartnerAddress "partnerID" "conID1337"
        it "Disconnect" $ serializesConsistentlyMessage $ Disconnect "ownID"
    describe "Serialize Responses" $ do
        it "Redirect" $ serializesConsistentlyResponse $ Redirect "42.0.0.1" "1337"
        it "Okay" $ serializesConsistentlyResponse Okay
        it "OkayIntroduce" $ serializesConsistentlyResponse $ OkayIntroduce "ownID"
        it "Wait" $ serializesConsistentlyResponse Wait
        it "Error" $ serializesConsistentlyResponse Error
    describe "Serialize ConversationSession" $ do
        it "ConversationMessage" $ serializesConsistentlyConversation $ ConversationMessage "conv42" $ NewValue "userID" 2 $ VInt 42
        it "ConversationResponse" $ serializesConsistentlyConversation $ ConversationResponse "conv42" Okay
        it "ConversationCloseAll" $ serializesConsistentlyConversation ConversationCloseAll