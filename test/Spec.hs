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


parse :: String -> [Decl]
parse = G.parseCalc . T.alexScanTokens

main :: IO ()
main = hspec $ do
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


