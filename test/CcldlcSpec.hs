{-# OPTIONS_GHC -Wall #-}

module CcldlcSpec (spec) where

import Test.Hspec
import Utils

import Syntax
import Kinds

spec :: Spec
spec =
  describe "CCLDGV parser for section2 examples" $ do
    it "f2' example function" $ do
      "val f2'  = 𝜆(x: *) 𝜆(y: case (x: * => Bool) {'T: Int, 'F: Bool}) case (x: * => Bool) {'T: 17+y, 'F: not y}"
      `shouldParseDecl`
       DFun "f2'" []
         (Lam MMany "x" TDyn
           (Lam MMany "y"
             (TCase (Cast "x" TDyn (TName False "Bool")) [("'T",TInt),("'F",TName False "Bool")])
             (Case (Cast "x" TDyn (TName False "Bool")) [("'T",Math (Add (Lit (LNat 17)) (Var "y"))),("'F",App (Var "not") (Var "y"))])
           )
         ) Nothing
