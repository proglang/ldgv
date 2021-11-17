{-# OPTIONS_GHC -Wall #-}
module InterpreterSpec (spec) where
import Test.Hspec
import Utils

import Kinds
import Syntax
import ProcessEnvironment

spec :: Spec
spec =
  describe "LDGV interpretation of single arithmetic declarations" $ do
    it "compares integer and double value" $ do
      VInt 42 == VDouble 42.0 `shouldBe` False
    it "interprets 12 + 56" $ do
      DFun "f" [] (Math $ Add (Lit $ LNat 12) (Lit $ LNat 56)) Nothing
      `shouldInterpretTo`
      VInt 68
    it "interprets 12.34 + 56.78" $ do
      DFun "f" [] (Math $ Add (Lit $ LDouble 12.34) (Lit $ LDouble 56.78)) Nothing
      `shouldInterpretTo`
      VDouble 69.12
    it "interprets 2.0 * (1.0 - 3.0) / 4.0" $ do
      DFun "f" [] (Math $ Div (Math $ Mul (Lit $ LDouble 2.0) (Math $ Sub (Lit $ LDouble 1.0) (Lit $ LDouble 3.0))) (Lit $ LDouble 4.0)) Nothing
      `shouldInterpretTo`
      VDouble (-1.0)

    it "interprets application of (x='T, y='T) on section2 example function f2'" $ do
      DFun "f2'" []
        (App (App
          (Lam MMany "x" TDyn
            (Lam MMany "y"
              (TCase (Cast "x" TDyn (TName False "Bool")) [("'T",TInt),("'F",TName False "Bool")])
              (Case (Cast "x" TDyn (TName False "Bool")) [("'T",Math (Add (Lit (LNat 17)) (Var "y"))),("'F",App (Var "not") (Var "y"))])
            )
          )
        (Lit (LLab "'T"))) (Lit (LLab "'T"))) Nothing
      `shouldInterpretTo`
      VLabel "'F"
