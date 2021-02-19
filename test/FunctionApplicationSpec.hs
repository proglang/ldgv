module FunctionApplicationSpec (spec) where
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Parsing
import qualified Tokens as T
import qualified Grammar as G
import Syntax
import Kinds

spec :: Spec
spec = 
  describe "LDGV parser function application and let tests" $ do
    it "parses simple function application" $ do
      parse "val f (g:Int) (n:Int) = g n" `shouldBe`
        [DFun "f" [(MMany,"g",TInt),(MMany,"n",TInt)] (App (Var "g") (Var "n")) Nothing]

    it "parses function application to two arguments" $ do
      parse "val f (g:Int) (n:Int) = g n n" `shouldBe`
        [DFun "f" [(MMany,"g",TInt),(MMany,"n",TInt)] (App (App (Var "g") (Var "n")) (Var "n")) Nothing]

    it "parses let around infix op" $ do
      parse "val f (m:Int) (n:Int) = let x = m in n + n" `shouldBe`
        [DFun "f" [(MMany,"m",TInt),(MMany,"n",TInt)]
         (Let "x" (Var "m")
          (Math $ Add (Var "n") (Var "n")))
         Nothing]

    it "parses nested let" $ do
      parse "val f (m:Int) (n:Int) = let x = m in let y = x in x + y" `shouldBe`
        [DFun "f" [(MMany,"m",TInt),(MMany,"n",TInt)] 
         (Let "x" (Var "m")
          (Let "y" (Var "x")
           (Math $ Add (Var "x") (Var "y"))))
          Nothing]

    it "parses nested let(pair)" $ do
      parse "val f (m:Int) (n:Int) = let <x,y> = m in let z = x in z + y + n" `shouldBe`
        [DFun "f" [(MMany,"m",TInt),(MMany,"n",TInt)] 
         (LetPair "x" "y" (Var "m")
          (Let "z" (Var "x")
           (Math $ Add (Math $ Add (Var "z") (Var "y")) (Var "n")))) 
          Nothing]

    it "parses nested let(pair)1" $ do
      parse "val sendNode (n : Node) (c : NodeC) = let < tag , v > = n in (let c = send c tag in   send c v)" `shouldBe`
        [DFun "sendNode" [(MMany,"n",TName False "Node"),(MMany,"c",TName False "NodeC")]
          (LetPair "tag" "v" (Var "n")
            (Let "c" (App (Send (Var "c")) (Var "tag")) 
             (App (Send (Var "c")) (Var "v"))))
          Nothing]

    it "parses nested let(pair)2" $ do
      parse "val sendNode (n : Node) (c : NodeC) = let < tag , v > = n in let c = send c tag in   send c v" `shouldBe`
        [DFun "sendNode" [(MMany,"n",TName False "Node"),(MMany,"c",TName False "NodeC")]
          (LetPair "tag" "v" (Var "n")
            (Let "c" (App (Send (Var "c")) (Var "tag")) 
             (App (Send (Var "c")) (Var "v"))))
          Nothing]


