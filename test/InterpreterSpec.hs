{-# OPTIONS_GHC -Wall #-}
module InterpreterSpec (spec) where
import Test.Hspec
import Utils

import Syntax
import ProcessEnvironment

spec :: Spec
spec =
  describe "LDGV interpretation of single arithmetic declarations" $ do
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
