{-# OPTIONS_GHC -Wall #-}
module InterpreterSpec (spec) where
import Test.Hspec
import Utils

import Kinds
import Syntax
import Interpreter
import ProcessEnvironment
import ProcessEnvironmentTypes
import UtilsFuncCcldlc

spec :: Spec
spec = do
  describe "LDGV interpretation of string expression and type" $ do
    it "interpret empty string literal" $
      [DFun "main" [] (Lit (LString "")) Nothing]
      `shouldInterpretTo`
      VString ""
    it "interpret simple string literal" $
      [DFun "main" [] (Lit (LString "foo bar")) Nothing]
      `shouldInterpretTo`
      VString "foo bar"

  describe "LDGV interpretation of single arithmetic declarations" $ do
    it "compares integer and double value" $
      VInt 42 == VDouble 42.0 `shouldBe` False
    it "interprets 12 + 56" $
      [DFun "main" [] (Math $ Add (Lit $ LNat 12) (Lit $ LNat 56)) Nothing]
      `shouldInterpretTo`
      VInt 68
    it "interprets 12.34 + 56.78" $
      [DFun "main" [] (Math $ Add (Lit $ LDouble 12.34) (Lit $ LDouble 56.78)) Nothing]
      `shouldInterpretTo`
      VDouble 69.12
    it "interprets 2.0 * (1.0 - 3.0) / 4.0" $
      [DFun "main" [] (Math $ Div (Math $ Mul (Lit $ LDouble 2.0) (Math $ Sub (Lit $ LDouble 1.0) (Lit $ LDouble 3.0))) (Lit $ LDouble 4.0)) Nothing]
      `shouldInterpretTo`
      VDouble (-1.0)

  describe "LDLC function interpretation" $ do
    it "interprets application of (x='F, y='F) on section2 example function f" $
      [ boolType
      , notFunc
      , DFun "main" [] (App (App f
        (Lit (LLab "'F"))) (Lit (LLab "'F"))) Nothing]
      `shouldInterpretTo`
      VLabel "'T"

  describe "LDLC pair interpretation" $ do
    it "interprets function returning a <Bool, Int> pair: <x='T, 1>" $
      [ boolType
      , DFun "main" [] (Pair MMany "x" (Lit (LLab "'T")) (Lit $ LNat 1)) Nothing]
      `shouldInterpretTo`
      VPair (VLabel "'T") (VInt 1)
    it "interprets function returning a <Bool, Int> pair, where snd depends on evaluation of fst: <x='T, case x {'T: 1, 'F: 0}>" $
      [ boolType
      , DFun "main" [] (Pair MMany "x" (Lit $ LLab "'T") (Case (Var "x") [("'T",Lit $ LNat 1), ("'F",Lit $ LNat 0)])) Nothing]
      `shouldInterpretTo`
      VPair (VLabel "'T") (VInt 1)

  describe "CCLDLC function interpretation" $ do
    it "interprets application of x='F, y=('F: Bool => *) on section2 example function f1'" $
      [ boolType
      , notFunc
      , DFun "main" [] (App (App f1' (Lit $ LLab "'F")) (Cast (Lit $ LLab "'F") (TName False "Bool") TDyn)) Nothing]
      `shouldInterpretTo`
      VLabel "'T"
    it "interprets application of x=('F: Bool => *), y='F on section2 example function f2'" $
      [ boolType
      , notFunc
      , DFun "main" [] (App (App f2' (Cast (Lit (LLab "'F")) (TName False "Bool") TDyn)) (Lit (LLab "'F"))) Nothing]
      `shouldInterpretTo`
      VLabel "'T"
    it "interprets application of x=('T: Bool -> *), y=6 on section2 example function f2'" $
      [ boolType
      , notFunc
      , DFun "main" [] (App (App f2' (Cast (Lit (LLab "'T")) (TName False "Bool") TDyn)) (Lit (LNat 6))) Nothing]
      `shouldInterpretTo`
      VInt 23
    it "interprets application of x=('F: MaybeBool => *), y='F on section2 example function f2'" $
      shouldThrowCastException
        [ boolType
        , notFunc
        , maybeBoolType
        , DFun "main" [] (App (App f2' (Cast (Lit (LLab "'F")) (TName False "MaybeBool") TDyn)) (Lit (LLab "'F"))) Nothing]
    it "interprets application of x=('F: MaybeBool => Bool), y='F on section2 example function f" $
      shouldThrowCastException
        [ boolType
        , notFunc
        , maybeBoolType
        , DFun "main" [] (App (App f (Cast (Lit (LLab "'F")) (TName False "MaybeBool") (TName False "Bool"))) (Lit (LLab "'F"))) Nothing]
    it "interprets application of x=('T: OnlyTrue => Bool), y='6 on section2 example function f" $
      [ boolType
      , notFunc
      , onlyTrueType
      , DFun "main" [] (App (App f (Cast (Lit (LLab "'T")) (TName False "OnlyTrue") (TName False "Bool"))) (Lit (LNat 6))) Nothing]
      `shouldInterpretTo`
      VInt 23
    it "interprets application of x=('T: OnlyTrue => *), y=6 on section2 example function f2'" $
      [ boolType
      , notFunc
      , onlyTrueType
      , DFun "main" [] (App (App f2' (Cast (Lit (LLab "'T")) (TName False "OnlyTrue") TDyn)) (Lit (LNat 6))) Nothing]
      `shouldInterpretTo`
      VInt 23
    it "interprets application of x='T, y=('L: Direction => *), z='T on example function f3" $
      [ boolType
      , notFunc
      , directionType
      , DFun "main" [] (App (App (App f3 (Lit (LLab "'T"))) (Cast (Lit (LLab "'R")) (TName False "Direction") TDyn)) (Lit (LLab "'T"))) Nothing]
      `shouldInterpretTo`
      VLabel "'F"
    it "interprets application of x='F,y=(not: (b:Bool) -> Bool => *),z='T on example function f4'" $
      [ boolType
      , notFunc
      , DFun "main" []
          (App (App (App f4'
            (Lit (LLab "'F")))
              (Cast (Var "not") (TFun MMany "b" (TName False "Bool") (TName False "Bool")) TDyn))
                (Lit (LLab "'T")))
          Nothing]
        `shouldInterpretTo`
        VLabel "'F"
    it "interprets application of x='T,y=(and: (a:Bool) -> (b:Bool) -> Bool => *),z='T on example function f4'" $
      [ boolType
      , andFunc
      , DFun "main" []
          (App (App (App f4'
            (Lit (LLab "'T")))
              (Cast (Var "and") (TFun MMany "a" (TName False "Bool")  (TFun MMany "b" (TName False "Bool") (TName False "Bool"))) TDyn))
                (Lit (LLab "'T")))
          Nothing]
      `shouldInterpretTo`
      VLabel "'T"
    it "interprets application of x='F,y=(not: (b:Bool) -> Bool => *) on example function f4" $
      [ boolType
      , notFunc
      , DFun "main" [] (App (App f4 (Lit $ LLab "'F")) (Cast (Var "not") (TFun MMany "b" (TName False "Bool") (TName False "Bool")) TDyn))
          Nothing]
      `shouldInterpretTo`
      VLabel "'T"
    it "interprets cast of boolean pair from Bool to OnlyTrue" $
      shouldThrowCastException
        [ boolType
        , onlyTrueType
        , DFun "main" []
          (Cast
            (Pair MMany "x"
              (Cast (Lit (LLab "'T")) (TName False "Bool") TDyn)
              (Case (Cast (Var "x") TDyn (TName False "Bool")) [("'T",Lit (LLab "'T")),("'F",Lit (LLab "'F"))]))
            (TPair "x" (TName False "Bool") (TName False "Bool"))
            (TPair "x" (TName False "OnlyTrue") (TName False "OnlyTrue")))
          Nothing]
    it "interprets cast of boolean pair from Bool to MaybeBool" $
      [ boolType
      , maybeBoolType
      , DFun "main" []
        (Cast
          (Pair MMany "x"
            (Cast (Lit (LLab "'T")) (TName False "Bool") TDyn)
            (Case (Cast (Var "x") TDyn (TName False "Bool")) [("'T",Lit (LLab "'T")),("'F",Lit (LLab "'F"))]))
          (TPair "x" (TName False "Bool") (TName False "Bool"))
          (TPair "x" (TName False "MaybeBool") (TName False "MaybeBool")))
        Nothing]
      `shouldInterpretTo`
      VPair (VDynCast (VLabel "'T") (GLabel (labelsFromList ["'T", "'F"]))) (VLabel "'T")
    it "interprets dynamic plus function with x=(5:Int->*),y=(8:Int->*)" $
      [DFun "main" []
        (App
          (App
            (Lam MMany "x" TDyn
              (Lam MMany "y" TDyn
                (Math (Add
                  (Cast (Var "x") TDyn TInt)
                  (Cast (Var "y") TDyn TInt)))))
            (Cast (Lit $ LInt 5) TInt TDyn))
          (Cast (Lit $ LInt 8) TInt TDyn))
        Nothing]
      `shouldInterpretTo`
      VInt 13
  describe "CCLDLC recursor expressions (rec, natrec, new_natrec)" $ do
    it "interprets sumf rec expression to itself" $
      [DFun "main" [] sumfRec Nothing]
      `shouldInterpretTo`
      VRec [] "f" "n1"
        (Lam MMany "acc" TInt
          (Lam MMany "x" TInt (App (App (Var "f") (Var "n1")) (Math (Add (Var "acc") (Var "x"))))))
        (Lam MMany "acc" TInt
          (Var "acc"))
    it "interprets application sumf 0 0 to M term, then to 0" $
      [DFun "main" []
        (App (App sumfRec (Lit $ LInt 0)) (Lit $ LInt 0))
        Nothing]
      `shouldInterpretTo`
      VInt 0
    it "interprets application sumf 1 1 1" $
      [DFun "main" []
        (App (App (App sumfRec (Lit $ LInt 1)) (Lit $ LInt 1)) (Lit $ LInt 1))
        Nothing]
      `shouldInterpretTo`
      VInt 2
    it "interprets application sumf 5 0 1 2 3 4 5" $
      [DFun "main" []
        (App (App (App (App (App (App (App sumfRec (Lit $ LInt 5)) (Lit $ LInt 0)) (Lit $ LInt 1)) (Lit $ LInt 2)) (Lit $ LInt 3)) (Lit $ LInt 4)) (Lit $ LInt 5))
        Nothing]
      `shouldInterpretTo`
      VInt 15
    it "interprets application sumf -1 expecting failure" $
      DFun "main" []
        (App sumfRec (Lit $ LInt (-1)))
        Nothing
      `shouldThrowInterpreterException`
      RecursorNotNatException
    it "interprets application newsumf 0 0 to M term, then to 0" $
      [DFun "main" []
        (App (App newsumfRec (Lit $ LInt 0)) (Lit $ LInt 0))
        Nothing]
      `shouldInterpretTo`
      VInt 0
    it "interprets application newsumf 1 1 1" $
      [DFun "main" []
        (App (App (App newsumfRec (Lit $ LInt 1)) (Lit $ LInt 1)) (Lit $ LInt 1))
        Nothing]
      `shouldInterpretTo`
      VInt 2
    it "evaluates NatRec type to head normal form" $
      TNatRec (Lit $ LNat 1) TInt "A" (TFun MMany "x" TInt (TName False "A"))
      `shouldInterpretTypeTo`
      -- NFFunc does not further evaluate its component types, therefore
      -- its environment contains an unevaluates TNatRec which is equivalent
      -- to TInt since its given Z (zero).
      NFFunc (FuncType [("A", VType lower)] "x" TInt (TName False "A"))
      where lower = TNatRec (Lit $ LNat 0) TInt "A" (TFun MMany "x" TInt (TName False "A"))
