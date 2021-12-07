{-# OPTIONS_GHC -Wall #-}

module CcldlcSpec (spec) where

import Test.Hspec
import Utils

import Syntax
import Kinds

spec :: Spec
spec =
  describe "CCLDGV parser for section2 examples" $ do
    it "f2' example function" $
      "val f2' = 𝜆(x: *) 𝜆(y: case (x: * => Bool) {'T: Int, 'F: Bool}) case (x: * => Bool) {'T: 17+y, 'F: not y}"
      `shouldParseDecl`
      DFun "f2'" []
        (Lam MMany "x" TDyn
          (Lam MMany "y"
            (TCase (Cast (Var "x") TDyn (TName False "Bool")) [("'T",TInt),("'F",TName False "Bool")])
            (Case (Cast (Var "x") TDyn (TName False "Bool")) [("'T",Math (Add (Lit (LNat 17)) (Var "y"))),("'F",App (Var "not") (Var "y"))])))
        Nothing
    it "f3 example function for (* => case ...)" $
      "val f3 = 𝜆(x: Bool) 𝜆(y: *) 𝜆(z: case (y: * => case x {'T: Direction, 'F: Bool}) {'T: Direction, 'F: Bool, 'L: Bool, 'R: Bool}) case (y: * => case x {'T: Direction, 'F: Bool}) {'T: y, 'F: not y, 'L: z, 'R: not z}"
      `shouldParseDecl`
      DFun "f3" []
        (Lam MMany "x" (TName False "Bool")
          (Lam MMany "y" TDyn
            (Lam MMany "z"
              (TCase (Cast (Var "y") TDyn (TCase (Var "x") [("'T",TName False "Direction"),("'F",TName False "Bool")]))
                [("'T",TName False "Direction"),("'F",TName False "Bool"),("'L",TName False "Bool"),("'R",TName False "Bool")])
                (Case (Cast (Var "y") TDyn (TCase (Var "x") [("'T",TName False "Direction"),("'F",TName False "Bool")]))
                  [("'T",Var "y"),("'F",App (Var "not") (Var "y")),("'L",Var "z"),("'R",App (Var "not") (Var "z"))]))))
        Nothing
