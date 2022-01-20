module UtilsFuncCcldlc
  ( module UtilsFuncCcldlc
  ) where

import Kinds
import Syntax

-- type Bool : ~un = {'T, 'F}
boolType :: Decl
boolType = DType "Bool" MMany Kun (TLab ["'T","'F"])

-- type MaybeBool : ~un = {'T, 'F, 'N}
maybeBoolType :: Decl
maybeBoolType = DType "MaybeBool" MMany Kun (TLab ["'T","'F","'N"])

-- type OnlyTrue : ~un = {'T}
onlyTrueType :: Decl
onlyTrueType = DType "OnlyTrue" MMany Kun (TLab ["'T"])

-- val not(b: Bool) = (case b {'T: 'F, 'F: 'T})
notFunc :: Decl
notFunc = DFun "not" [(MMany,"b",TName False "Bool")] (Case (Var "b") [("'T",Lit (LLab "'F")),("'F",Lit (LLab "'T"))]) Nothing

-- val and(a: Bool, b: Bool) = (case a {'T: b, 'F: 'F})
andFunc :: Decl
andFunc = DFun "and" [(MMany,"a",TName False "Bool"),(MMany,"b",TName False "Bool")] (Case (Var "a") [("'T",Var "b"),("'F",Lit (LLab "'F"))]) Nothing

-- val f = 𝜆(x: Bool) 𝜆(y: case x {'T: Int, 'F: Bool}) case x {'T: 17+y, 'F: not y}
f :: Exp
f = Lam MMany "x" (TName False "Bool")
  (Lam MMany "y"
    (TCase (Var "x") [("'T",TInt),("'F",TName False "Bool")])
    (Case (Var "x") [("'T",Math (Add (Lit (LNat 17)) (Var "y"))) ,("'F",App (Var "not") (Var "y"))]))

-- val f1' = 𝜆(x: Bool) 𝜆(y: *) case x {'T: 17 + (y: * => Int), 'F: not (y: * => Bool)}
f1' :: Exp
f1' = Lam MMany "x" (TName False "Bool")
  (Lam MMany "y" TDyn (Case (Var "x")
    [("'T",Math (Add (Lit (LNat 17)) (Cast (Var "y") TDyn TInt)))
    ,("'F",App (Var "not") (Cast (Var "y") TDyn (TName False "Bool")))]))

-- val f2' = 𝜆(x: *) 𝜆(y: case (x: * => Bool) {'T: Int, 'F: Bool}) case (x: * => Bool) {'T: 17+y, 'F: not y}
f2' :: Exp
f2' = Lam MMany "x" TDyn
  (Lam MMany "y"
    (TCase (Cast (Var "x") TDyn (TName False "Bool")) [("'T",TInt),("'F",TName False "Bool")])
    (Case (Cast (Var "x") TDyn (TName False "Bool")) [("'T",Math (Add (Lit (LNat 17)) (Var "y"))),("'F",App (Var "not") (Var "y"))]))

-- type Direction : ~un = {'L, 'R}
directionType :: Decl
directionType = DType "Direction" MMany Kun (TLab ["'L","'R"])

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

-- val f4' = 𝜆(x: Bool) 𝜆(y: *) 𝜆(z: Bool) case x {'T: (y: * => (a:Bool) -> (b:Bool) -> Bool) z z, 'F: (y: * => (b:Bool) -> Bool) z}
f4' :: Exp
f4' = Lam MMany "x" (TName False "Bool")
  (Lam MMany "y" TDyn
    (Lam MMany "z" (TName False "Bool")
      (Case (Var "x")
        [("'T",App (App (Cast (Var "y") TDyn (TFun MMany "a" (TName False "Bool") (TFun MMany "b" (TName False "Bool") (TName False "Bool")))) (Var "z")) (Var "z"))
        ,("'F",App (Cast (Var "y") TDyn (TFun MMany "b" (TName False "Bool") (TName False "Bool"))) (Var "z"))])))
