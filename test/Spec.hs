import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import qualified Tokens as T
import qualified Grammar as G
import Syntax
import Kinds

example_head :: IO ()
example_head = hspec $ do
  describe "Prelude.head" $ do
    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)

    it "returns the first element of an *arbitrary* list" $
      property $ \x xs -> head (x:xs) == (x :: Int)

    it "throws an exception if used with an empty list" $ do
      evaluate (head []) `shouldThrow` anyException

scan :: String -> [T.Token]
scan = T.alexScanTokens

scanTest =
  describe "LDGV scanner" $ do
    it "scans an integer" $ do
      scan "42" `shouldBe`
        [T.Int 42]
    it "scans a double" $ do
      scan "-3.14159" `shouldBe`
        [T.Double (negate 3.14159)]
    it "scans another double" $ do
      scan "-0.0" `shouldBe`
        [T.Double (negate 0.0)]
    it "scans an empty string" $ do
      scan "\"\"" `shouldBe`
        [T.Str ""]
    it "scans alphanumeric string" $ do
      scan "\"abhcg3555\"" `shouldBe`
        [T.Str "abhcg3555"]
    it "scans numeric string" $ do
      scan "\"4711lskdfj\"" `shouldBe`
        [T.Str "4711lskdfj"]
    it "scans string with whitespace" $ do
      scan "\"I found salvation\"" `shouldBe`
        [T.Str "I found salvation"]
    it "scans string with special symbols" $ do
      scan "\". ; , $|*abs#?~-{}[]/\"" `shouldBe`
        [T.Str ". ; , $|*abs#?~-{}[]/"]
    it "scans string with double quote" $ do
      scan "\"affe\\\"nschande\"" `shouldBe`
        [T.Str "affe\"nschande"]

parse :: String -> [Decl]
parse = G.parseCalc . T.alexScanTokens

parseTest = 
  describe "LDGV parser" $ do
    it "parses an addition" $ do
      parse "val f (m:Int) (n:Int) = m + n" `shouldBe`
        [DFun "f" [(MMany,"m",TInt),(MMany,"n",TInt)] (Plus (Var "m") (Var "n")) Nothing]

    it "parses a subtraction" $ do
      parse "val f (m:Int) (n:Int) = m - n" `shouldBe`
        [DFun "f" [(MMany,"m",TInt),(MMany,"n",TInt)] (Minus (Var "m") (Var "n")) Nothing]

    it "parses a negation" $ do
      parse "val f (m:Int) (n:Int) = - n" `shouldBe`
        [DFun "f" [(MMany,"m",TInt),(MMany,"n",TInt)] (Negate (Var "n")) Nothing]

    it "parses a negation with multiplication" $ do
      parse "val f (m:Int) (n:Int) = - 2 * n" `shouldBe`
        [DFun "f" [(MMany,"m",TInt),(MMany,"n",TInt)] (Negate (Times (Nat 2) (Var "n"))) Nothing]

    it "parses a negation with subtraction" $ do
      parse "val f (m:Int) (n:Int) = - 2 - n" `shouldBe`
        [DFun "f" [(MMany,"m",TInt),(MMany,"n",TInt)] (Minus (Negate (Nat 2)) (Var "n")) Nothing]

    it "parses a double negation" $ do
      parse "val f (m:Int) (n:Int) = - - n" `shouldBe`
        [DFun "f" [(MMany,"m",TInt),(MMany,"n",TInt)] (Negate (Negate (Var "n"))) Nothing]

    it "parses precedence of multiplication left over subtraction" $ do
      parse "val f (m:Int) (n:Int) = m - 2 * n" `shouldBe`
        [DFun "f" [(MMany,"m",TInt),(MMany,"n",TInt)] (Minus (Var "m") (Times (Nat 2) (Var "n"))) Nothing]

    it "parses precedence of multiplication right over subtraction" $ do
      parse "val f (m:Int) (n:Int) = m * 2 - n" `shouldBe`
        [DFun "f" [(MMany,"m",TInt),(MMany,"n",TInt)] (Minus (Times (Var "m") (Nat 2)) (Var "n")) Nothing]

    it "parses simple function application" $ do
      parse "val f (g:Int) (n:Int) = g n" `shouldBe`
        [DFun "f" [(MMany,"g",TInt),(MMany,"n",TInt)] (App (Var "g") (Var "n")) Nothing]

    it "parses function application to two arguments" $ do
      parse "val f (g:Int) (n:Int) = g n n" `shouldBe`
        [DFun "f" [(MMany,"g",TInt),(MMany,"n",TInt)] (App (App (Var "g") (Var "n")) (Var "n")) Nothing]

    it "parses let around infix op" $ do
      parse "val f (m:Int) (n:Int) = let x = m in n + n" `shouldBe`
        [DFun "f" [(MMany,"m",TInt),(MMany,"n",TInt)] (Let "x" (Var "m") (Plus (Var "n") (Var "n"))) Nothing]

    it "parses nested let" $ do
      parse "val f (m:Int) (n:Int) = let x = m in let y = x in x + y" `shouldBe`
        [DFun "f" [(MMany,"m",TInt),(MMany,"n",TInt)] 
         (Let "x" (Var "m")
          (Let "y" (Var "x")
           (Plus (Var "x") (Var "y"))))
          Nothing]

    it "parses nested let(pair)" $ do
      parse "val f (m:Int) (n:Int) = let <x,y> = m in let z = x in z + y + n" `shouldBe`
        [DFun "f" [(MMany,"m",TInt),(MMany,"n",TInt)] 
         (LetPair "x" "y" (Var "m")
          (Let "z" (Var "x")
           (Plus (Plus (Var "z") (Var "y")) (Var "n")))) 
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

    it "parses a case without 'of'" $ do
      parse "val iors(x:*)=case x{'True: 42, 'False: ()}" `shouldBe`
        [DFun "iors" [(MMany, "x",TDyn)] (Case (Var "x") [("True",Nat 42), ("False", Unit)]) Nothing]

    it "parses a case with 'of'" $ do
      parse "val iors(x:*) = case x of{'True: 42, 'False: ()}" `shouldBe`
        [DFun "iors" [(MMany, "x",TDyn)] (Case (Var "x") [("True",Nat 42), ("False", Unit)]) Nothing]

    it "parses a dynamic type" $ do
      parse "val f (m:*) = m" `shouldBe`
        [DFun "f" [(MMany,"m",TDyn)] (Var "m") Nothing]

main :: IO ()
main = hspec $ do
  scanTest
  parseTest
