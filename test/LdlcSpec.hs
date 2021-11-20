{-# OPTIONS_GHC -Wall #-}

module LdlcSpec (spec) where

import Test.Hspec
import Utils

import Syntax
import Kinds

spec :: Spec
spec =
  describe "LDGV parser for section2 examples" $ do
    it "type declaration for bool" $ do
      "type Bool : ~un = {'T, 'F}"
      `shouldParseDecl`
      DType "Bool" MMany Kun (TLab ["'T","'F"])
    it "type declaration for function not :: Bool -> Bool" $ do
      "val not(b: Bool) = (case b {'T: 'F, 'F: 'T})"
      `shouldParseDecl`
      DFun "not" [(MMany,"b",TName False "Bool")]
        (Case (Var "b")
          [("'T",Lit (LLab "'F"))
          ,("'F",Lit (LLab "'T"))])
        Nothing
    it "f example function" $ do
      "val f = 𝜆(x: Bool) 𝜆(y: case x {'T: Int, 'F: Bool}) case x {'T: 17+y, 'F: not y}"
      `shouldParseDecl`
      DFun "f" []
        (Lam MMany "x" (TName False "Bool")
          (Lam MMany "y" (TCase (Var "x") [("'T",TInt),("'F",TName False "Bool")])
            (Case (Var "x")
              [("'T",Math (Add (Lit (LNat 17)) (Var "y")))
              ,("'F",App (Var "not") (Var "y"))])))
        Nothing
