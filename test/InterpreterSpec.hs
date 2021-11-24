{-# OPTIONS_GHC -Wall #-}
module InterpreterSpec (spec) where
import Test.Hspec
import Utils

import Kinds
import Syntax
import ProcessEnvironment

spec :: Spec
spec = do
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

  describe "LDLC function interpretation" $ do
    it "interprets application of (x='F, y='F) on section2 example function f" $ do
      DFun "f" []
        (App (App
          (Lam MMany "x" (TName False "Bool")
            (Lam MMany "y" (TCase (Var "x") [("'T",TInt),("'F",TName False "Bool")])
              (Case (Var "x")
                [("'T",Math (Add (Lit (LNat 17)) (Var "y")))
                ,("'F",App (Var "not") (Var "y"))])))
          (Lit (LLab "'F"))) (Lit (LLab "'F")))
        Nothing
      `shouldInterpretTo`
      VLabel "'T"

  describe "CCLDLC function interpretation" $ do
    -- val f2'  = 𝜆(x: *) 𝜆(y: case (x: * => Bool) {'T: Int, 'F: Bool}) case (x: * => Bool) {'T: 17+y, 'F: not y}
    let f2' = Lam MMany "x" TDyn
                (Lam MMany "y"
                  (TCase (Cast (Var "x") TDyn (TName False "Bool")) [("'T",TInt),("'F",TName False "Bool")])
                  (Case (Cast (Var "x") TDyn (TName False "Bool")) [("'T",Math (Add (Lit (LNat 17)) (Var "y"))),("'F",App (Var "not") (Var "y"))]))
    it "interprets application of (x='F, y='F) on section2 example function f2'" $ do
      DFun "f2'" []
        (App
          (App f2'
            (Cast (Lit (LLab "'F")) (TName False "Bool") TDyn))
          (Lit (LLab "'F")))
        Nothing
      `shouldInterpretTo`
      VLabel "'T"
    it "interprets application of (x='T, y=6) on section2 example function f2'" $ do
      DFun "f2'" []
        (App
          (App f2'
            (Lit (LLab "'T")))
          (Lit (LNat 6)))
        Nothing
      `shouldInterpretTo`
      VInt 23
