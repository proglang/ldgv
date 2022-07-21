module CcldlcSpec (spec) where

import Test.Hspec
import Utils
import UtilsFuncCcldlc

import Syntax
import Kinds

spec :: Spec
spec = do
  describe "CCLDGV parser for section2 examples" $ do
    it "f1' example function" $
      "val f1' = 𝜆(x: Bool) 𝜆(y: *) case x {'T: 17 + (y: * => Int), 'F: not (y: * => Bool)}"
      `shouldParseDecl`
      DFun "f1'" [] f1' Nothing
    it "f2' example function" $
      "val f2' = 𝜆(x: *) 𝜆(y: case (x: * => Bool) {'T: Int, 'F: Bool}) case (x: * => Bool) {'T: 17+y, 'F: not y}"
      `shouldParseDecl`
      DFun "f2'" [] f2' Nothing
    it "f3 example function for (* => case ...)" $
      "val f3 = 𝜆(x: Bool) 𝜆(y: *) 𝜆(z: case (y: * => case x {'T: Direction, 'F: Bool}) {'T: Direction, 'F: Bool, 'L: Bool, 'R: Bool}) case (y: * => case x {'T: Direction, 'F: Bool}) {'T: y, 'F: not y, 'L: z, 'R: not z}"
      `shouldParseDecl`
      DFun "f3" [] f3 Nothing
    it "f4' example function for conversion from * to function type" $
      "val f4' = 𝜆(x: Bool) 𝜆(y: *) 𝜆(z: Bool) case x {'T: (y: * => (u:Bool) -> (v:Bool) -> Bool) z z, 'F: (y: * => (u:Bool) -> Bool) z}"
      `shouldParseDecl`
      DFun "f4'" [] f4' Nothing
    it "f6 example for pair cast and evaluation" $
      "val paircast = (<x=('T: Bool => *), case (x: * => Bool) {'T: 'T, 'F: 'F}> : [x : Bool, Bool] => [x : OnlyTrue, OnlyTrue])"
      `shouldParseDecl`
      DFun "paircast" []
        (Cast
          (Pair MMany "x"
            (Cast (Lit (LLab "'T")) (TName False "Bool") TDyn)
            (Case (Cast (Var "x") TDyn (TName False "Bool")) [("'T",Lit (LLab "'T")),("'F",Lit (LLab "'F"))]))
          (TPair "x" (TName False "Bool") (TName False "Bool"))
          (TPair "x" (TName False "OnlyTrue") (TName False "OnlyTrue")))
        Nothing
    it "example term (2) from section 5.1 does fail when typechecking subtyping" $
      "(x: Unit) -> Int <: (x: *) -> case (x: * => Bool) {'T: Int, 'F: Bool}"
      `shouldParseDecl`
      DSub (TFun MMany "x" TUnit TInt) (TFun MMany "x" TDyn (TCase (Cast (Var "x") TDyn (TName False "Bool")) [("'T",TInt),("'F",TName False "Bool")]))
    it "simple dynamic cast involving Int" $ do
      "val plus = 𝜆(x: *) 𝜆(y: Int) ((x:*=>Int) + (y:*=>Int))"
      `shouldParseDecl`
      DFun "plus" []
        (Lam MMany "x" TDyn
          (Lam MMany "y" TInt
            (Math (Add
              (Cast (Var "x") TDyn TInt)
              (Cast (Var "y") TDyn TInt)))))
        Nothing
  describe "recursor expressions (natrec, new_natrec, rec)" $ do
    it "sumf function using rec" $
      "val sumf = rec f ( n1 . (fn (acc : Int) fn (x : Int) f n1 (acc + x)) ) (fn (acc : Int) acc)"
      `shouldParseDecl`
      DFun "sumf" [] sumfRec Nothing
    it "newsumf function using new_natrec" $
      "val newsumf = new_natrec f : n1 . A . (acc : Int) -> A { fn (acc: Int) acc, n1. fn (acc : Int) fn (x : Int) f n1 (acc + x) }"
      `shouldParseDecl`
      DFun "newsumf" [] newsumfRec Nothing
