{-# OPTIONS_GHC -Wall #-}
module FunctionApplicationSpec (spec) where
import Test.Hspec
import Utils

import Syntax
import Kinds

spec :: Spec
spec = 
  describe "LDGV parser function application and let tests" $ do
    it "parses simple function application" $ do
      "val f (g:Int) (n:Int) = g n" `shouldParseDecl`
        DFun "f" [(MMany,"g",TInt),(MMany,"n",TInt)]
          (App (Var "g") (Var "n"))
          Nothing

    it "parses function application to two arguments" $ do
      "val f (g:Int) (n:Int) = g n n" `shouldParseDecl`
        DFun "f" [(MMany,"g",TInt),(MMany,"n",TInt)]
          (App (App (Var "g") (Var "n")) (Var "n"))
          Nothing

    it "parses let around infix op" $ do
      "val f (m:Int) (n:Int) = let x = m in n + n" `shouldParseDecl`
        DFun "f" [(MMany,"m",TInt),(MMany,"n",TInt)]
         (Let "x" (Var "m")
          (Math $ Add (Var "n") (Var "n")))
         Nothing

    it "parses nested let" $ do
      "val f (m:Int) (n:Int) = let x = m in let y = x in x + y" `shouldParseDecl`
        DFun "f" [(MMany,"m",TInt),(MMany,"n",TInt)] 
         (Let "x" (Var "m")
          (Let "y" (Var "x")
           (Math $ Add (Var "x") (Var "y"))))
          Nothing

    it "parses nested let(pair)" $ do
      "val f (m:Int) (n:Int) = let <x,y> = m in let z = x in z + y + n" `shouldParseDecl`
        DFun "f" [(MMany,"m",TInt),(MMany,"n",TInt)] 
         (LetPair "x" "y" (Var "m")
          (Let "z" (Var "x")
           (Math $ Add (Math $ Add (Var "z") (Var "y")) (Var "n")))) 
          Nothing

    it "parses nested let(pair)1" $ do
      "val sendNode (n : Node) (c : NodeC) = let < tag , v > = n in (let c = send c tag in   send c v)" `shouldParseDecl`
        DFun "sendNode" [(MMany,"n",TName False "Node"),(MMany,"c",TName False "NodeC")]
          (LetPair "tag" "v" (Var "n")
            (Let "c" (App (Send (Var "c")) (Var "tag")) 
             (App (Send (Var "c")) (Var "v"))))
          Nothing

    it "parses nested let(pair)2" $ do
      "val sendNode (n : Node) (c : NodeC) = let < tag , v > = n in let c = send c tag in   send c v" `shouldParseDecl`
        DFun "sendNode" [(MMany,"n",TName False "Node"),(MMany,"c",TName False "NodeC")]
          (LetPair "tag" "v" (Var "n")
            (Let "c" (App (Send (Var "c")) (Var "tag")) 
             (App (Send (Var "c")) (Var "v"))))
          Nothing
