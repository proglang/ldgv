{-# OPTIONS_GHC -Wall #-}

module ArithmeticSpec (spec) where

import Test.Hspec
import Utils

import Syntax
import Kinds

spec :: Spec
spec =
  describe "LDGV parser arithmetic tests" $ do
    it "parses an addition" $ do
      "val f (m:Int) (n:Int) = m + n" `shouldParseDecl`
        DFun "f" [(MMany,"m",TInt),(MMany,"n",TInt)]
         (Math $ Add (Var "m") (Var "n"))
         Nothing

    it "parses a subtraction" $ do
      "val f (m:Int) (n:Int) = m - n" `shouldParseDecl`
        DFun "f" [(MMany,"m",TInt),(MMany,"n",TInt)]
         (Math $ Sub (Var "m") (Var "n"))
         Nothing

    it "parses a negation" $ do
      "val f (m:Int) (n:Int) = - n" `shouldParseDecl`
        DFun "f" [(MMany,"m",TInt),(MMany,"n",TInt)]
         (Math $ Neg (Var "n"))
         Nothing

    it "parses a negation with multiplication" $ do
      "val f (m:Int) (n:Int) = - 2 * n" `shouldParseDecl`
        DFun "f" [(MMany,"m",TInt),(MMany,"n",TInt)]
         (Math $ Neg (Math $ Mul (Lit $ LNat 2) (Var "n")))
         Nothing

    it "parses a negation with subtraction" $ do
      "val f (m:Int) (n:Int) = - 2 - n" `shouldParseDecl`
        DFun "f" [(MMany,"m",TInt),(MMany,"n",TInt)]
         (Math $ Sub (Math $ Neg (Lit $ LNat 2)) (Var "n"))
         Nothing

    it "parses a double negation" $ do
      "val f (m:Int) (n:Int) = - - n" `shouldParseDecl`
        DFun "f" [(MMany,"m",TInt),(MMany,"n",TInt)]
         (Math $ Neg (Math $ Neg (Var "n")))
         Nothing

    it "parses precedence of multiplication left over subtraction" $ do
      "val f (m:Int) (n:Int) = m - 2 * n" `shouldParseDecl`
        DFun "f" [(MMany,"m",TInt),(MMany,"n",TInt)]
         (Math $ Sub (Var "m") (Math $ Mul (Lit $ LNat 2) (Var "n")))
         Nothing

    it "parses precedence of multiplication right over subtraction" $ do
      "val f (m:Int) (n:Int) = m * 2 - n" `shouldParseDecl`
        DFun "f" [(MMany,"m",TInt),(MMany,"n",TInt)]
         (Math $ Sub (Math $ Mul (Var "m") (Lit $ LNat 2)) (Var "n"))
         Nothing

    it "parses a division" $ do
      "val f (m:Int) (n:Int) = m / n" `shouldParseDecl`
        DFun "f" [(MMany,"m",TInt),(MMany,"n",TInt)]
          (Math $ Div (Var "m") (Var "n"))
          Nothing

    it "parses substraction of negative double literals" $ do
      "val f = -12.34 - -56.78" `shouldParseDecl`
        DFun "f" [] (Math $ Sub
          (Math (Neg (Lit $ LDouble 12.34)))
          (Math (Neg (Lit $ LDouble 56.78))))
          Nothing

    it "parses addition of explicitly positive double literals" $ do
      "val f = +12.34 + +56.78" `shouldParseDecl`
        DFun "f" [] (Math $ Add
          (Lit $ LDouble 12.34) (Lit $ LDouble 56.78))
          Nothing

    it "parses substraction of negative integer literals" $ do
      "val f = -12 - -56" `shouldParseDecl`
        DFun "f" [] (Math $ Sub
          (Math (Neg (Lit $ LNat 12)))
          (Math (Neg (Lit $ LNat 56))))
          Nothing

    it "parses addition of explicitly positive integer literals" $ do
      "val f = +12 + +56" `shouldParseDecl`
        DFun "f" [] (Math $ Add
          (Lit $ LNat 12) (Lit $ LNat 56))
          Nothing
