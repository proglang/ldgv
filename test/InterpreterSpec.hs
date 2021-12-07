{-# OPTIONS_GHC -Wall #-}
module InterpreterSpec (spec) where
import Test.Hspec
import Utils

import Kinds
import Syntax
import ProcessEnvironment

-- type Bool : ~un = {'T, 'F}
boolTypeDecl :: Decl
boolTypeDecl = DType "Bool" MMany Kun (TLab ["'T","'F"])
-- type MaybeBool : ~un = {'T, 'F, 'N}
maybeBoolTypeDecl :: Decl
maybeBoolTypeDecl = DType "MaybeBool" MMany Kun (TLab ["'T","'F","'N"])
-- type OnlyTrue : ~un = {'T}
onlyTrueTypeDecl :: Decl
onlyTrueTypeDecl = DType "OnlyTrue" MMany Kun (TLab ["'T"])
-- val not(b: Bool) = (case b {'T: 'F, 'F: 'T})
notFuncDecl :: Decl
notFuncDecl = DFun "not" [(MMany,"b",TName False "Bool")] (Case (Var "b") [("'T",Lit (LLab "'F")),("'F",Lit (LLab "'T"))]) Nothing
-- val f = 𝜆(x: Bool) 𝜆(y: case x {'T: Int, 'F: Bool}) case x {'T: 17+y, 'F: not y}
f :: Exp
f = Lam MMany "x" (TName False "Bool")
  (Lam MMany "y"
    (TCase (Var "x") [("'T",TInt),("'F",TName False "Bool")])
    (Case (Var "x") [("'T",Math (Add (Lit (LNat 17)) (Var "y"))) ,("'F",App (Var "not") (Var "y"))]))
-- val f2' = 𝜆(x: *) 𝜆(y: case (x: * => Bool) {'T: Int, 'F: Bool}) case (x: * => Bool) {'T: 17+y, 'F: not y}
f2' :: Exp
f2' = Lam MMany "x" TDyn
  (Lam MMany "y"
    (TCase (Cast (Var "x") TDyn (TName False "Bool")) [("'T",TInt),("'F",TName False "Bool")])
    (Case (Cast (Var "x") TDyn (TName False "Bool")) [("'T",Math (Add (Lit (LNat 17)) (Var "y"))),("'F",App (Var "not") (Var "y"))]))
-- type Direction : ~un = {'L, 'R}
directionTypeDecl :: Decl
directionTypeDecl = DType "Direction" MMany Kun (TLab ["'L","'R"])
-- val f3 = 𝜆(x: Bool) 𝜆(y: *) 𝜆(z: case (y: * => case x {'T: Direction, 'F: Bool}) {'T: Direction, 'F: Bool, 'L: Bool, 'R: Bool})
--            case (y: * => case x {'T: Direction, 'F: Bool}) {'T: y, 'F: not y, 'L: z, 'R: not z}
f3 :: Exp
f3 = Lam MMany "x" (TName False "Bool")
  (Lam MMany "y" TDyn
    (Lam MMany "z"
      (TCase (Cast (Var "y") TDyn (TCase (Var "x") [("'T",TName False "Direction"),("'F",TName False "Bool")]))
        [("'T",TName False "Direction"),("'F",TName False "Bool"),("'L",TName False "Bool"),("'R",TName False "Bool")])
        (Case (Cast (Var "y") TDyn (TCase (Var "x") [("'T",TName False "Direction"),("'F",TName False "Bool")]))
          [("'T",Var "y"),("'F",App (Var "not") (Var "y")),("'L",Var "z"),("'R",App (Var "not") (Var "z"))])))

spec :: Spec
spec = do
  describe "LDGV interpretation of single arithmetic declarations" $ do
    it "compares integer and double value" $
      VInt 42 == VDouble 42.0 `shouldBe` False
    it "interprets 12 + 56" $
      DFun "f" [] (Math $ Add (Lit $ LNat 12) (Lit $ LNat 56)) Nothing
      `shouldInterpretTo`
      VInt 68
    it "interprets 12.34 + 56.78" $
      DFun "f" [] (Math $ Add (Lit $ LDouble 12.34) (Lit $ LDouble 56.78)) Nothing
      `shouldInterpretTo`
      VDouble 69.12
    it "interprets 2.0 * (1.0 - 3.0) / 4.0" $
      DFun "f" [] (Math $ Div (Math $ Mul (Lit $ LDouble 2.0) (Math $ Sub (Lit $ LDouble 1.0) (Lit $ LDouble 3.0))) (Lit $ LDouble 4.0)) Nothing
      `shouldInterpretTo`
      VDouble (-1.0)

  describe "LDLC function interpretation" $ do
    it "interprets application of (x='F, y='F) on section2 example function f" $
      shouldInterpretInPEnvTo [boolTypeDecl, notFuncDecl]
       (DFun "f" [] (App (App f
          (Lit (LLab "'F"))) (Lit (LLab "'F"))) Nothing)
        (VLabel "'T")

  describe "CCLDLC function interpretation" $ do
    it "interprets application of x=('F: Bool => *), y='F on section2 example function f2'" $
      shouldInterpretInPEnvTo [boolTypeDecl, notFuncDecl]
        (DFun "f2'" [] (App (App f2' (Cast (Lit (LLab "'F")) (TName False "Bool") TDyn)) (Lit (LLab "'F"))) Nothing)
        (VLabel "'T")
    it "interprets application of x=('T: Bool -> *), y=6 on section2 example function f2'" $
      shouldInterpretInPEnvTo [boolTypeDecl, notFuncDecl]
        (DFun "f2'" [] (App (App f2' (Cast (Lit (LLab "'T")) (TName False "Bool") TDyn)) (Lit (LNat 6))) Nothing)
        (VInt 23)
    it "interprets application of x=('F: MaybeBool => *), y='F on section2 example function f2'" $
      shouldInterpretInPEnvTo [boolTypeDecl, notFuncDecl, maybeBoolTypeDecl]
        (DFun "f2'" [] (App (App f2' (Cast (Lit (LLab "'F")) (TName False "MaybeBool") TDyn)) (Lit (LLab "'F"))) Nothing)
        (VLabel "'T")
    it "interprets application of x=('F: MaybeBool => Bool), y='F on section2 example function f" $
      shouldInterpretInPEnvTo [boolTypeDecl, notFuncDecl, maybeBoolTypeDecl]
        (DFun "f" [] (App (App f (Cast (Lit (LLab "'F")) (TName False "MaybeBool") (TName False "Bool"))) (Lit (LLab "'F"))) Nothing)
        (VLabel "'T")
    it "interprets application of x=('T: OnlyTrue => Bool), y='6 on section2 example function f expecting blame" $
      shouldThrowCastException [boolTypeDecl, notFuncDecl, onlyTrueTypeDecl]
        (DFun "f" [] (App (App f (Cast (Lit (LLab "'T")) (TName False "OnlyTrue") (TName False "Bool"))) (Lit (LNat 6))) Nothing)
    it "interprets application of x=('T: OnlyTrue => *), y=6 on section2 example function f2' expecting blame" $
      shouldThrowCastException [boolTypeDecl, notFuncDecl, onlyTrueTypeDecl]
        (DFun "f2'" [] (App (App f2' (Cast (Lit (LLab "'T")) (TName False "OnlyTrue") TDyn)) (Lit (LNat 6))) Nothing)
    it "interprets application of x='T, y=('L: Direction => *), z='T on example function f3" $
      shouldInterpretInPEnvTo [boolTypeDecl, notFuncDecl, directionTypeDecl]
        (DFun "f3" [] (App (App (App f3 (Lit (LLab "'T"))) (Cast (Lit (LLab "'R")) (TName False "Direction") TDyn)) (Lit (LLab "'T"))) Nothing)
        (VLabel "'F")
