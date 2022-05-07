{-# OPTIONS_GHC -Wno-missing-signatures #-}

module UtilsFuncCcldlc
  ( module UtilsFuncCcldlc
  ) where

import Kinds
import Syntax
import ProcessEnvironment

-- type Bool : ~un = {'T, 'F}
boolType = DType "Bool" MMany Kun (TLab ["'T","'F"])
boolTypeVal = ("Bool", VType $ TLab ["'T","'F"])

-- type MaybeBool : ~un = {'T, 'F, 'N}
maybeBoolType = DType "MaybeBool" MMany Kun (TLab ["'T","'F","'N"])
maybeBoolTypeVal = ("MaybeBool", VType $ TLab ["'T","'F","'N"])

-- type OnlyTrue : ~un = {'T}
onlyTrueType = DType "OnlyTrue" MMany Kun (TLab ["'T"])
onlyTrueTypeVal = ("OnlyTrue", VType $ TLab ["'T"])

-- val not(b: Bool) = (case b {'T: 'F, 'F: 'T})
notFunc = DFun "not" [(MMany,"b",TName False "Bool")] (Case (Var "b") [("'T",Lit (LLab "'F")),("'F",Lit (LLab "'T"))]) Nothing
notFuncVal = ("not", VFunc [] "b" (Case (Var "b") [("'T",Lit (LLab "'F")),("'F",Lit (LLab "'T"))]))

-- val and(a: Bool, b: Bool) = (case a {'T: b, 'F: 'F})
andFunc = DFun "and" [(MMany,"a",TName False "Bool"),(MMany,"b",TName False "Bool")] (Case (Var "a") [("'T",Var "b"),("'F",Lit (LLab "'F"))]) Nothing
andFuncVal = ("and", VFunc [] "a" (Lam MMany "b" (TName False "Bool") (Case (Var "a") [("'T",Var "b"),("'F",Lit (LLab "'F"))])))

-- val f = 𝜆(x: Bool) 𝜆(y: case x {'T: Int, 'F: Bool}) case x {'T: 17+y, 'F: not y}
f = Lam MMany "x" (TName False "Bool")
  (Lam MMany "y"
    (TCase (Var "x") [("'T",TInt),("'F",TName False "Bool")])
    (Case (Var "x") [("'T",Math (Add (Lit (LNat 17)) (Var "y"))) ,("'F",App (Var "not") (Var "y"))]))

-- val f1' = 𝜆(x: Bool) 𝜆(y: *) case x {'T: 17 + (y: * => Int), 'F: not (y: * => Bool)}
f1' = Lam MMany "x" (TName False "Bool")
  (Lam MMany "y" TDyn (Case (Var "x")
    [("'T",Math (Add (Lit (LNat 17)) (Cast (Var "y") TDyn TInt)))
    ,("'F",App (Var "not") (Cast (Var "y") TDyn (TName False "Bool")))]))

-- val f2' = 𝜆(x: *) 𝜆(y: case (x: * => Bool) {'T: Int, 'F: Bool}) case (x: * => Bool) {'T: 17+y, 'F: not y}
f2' = Lam MMany "x" TDyn
  (Lam MMany "y"
    (TCase (Cast (Var "x") TDyn (TName False "Bool")) [("'T",TInt),("'F",TName False "Bool")])
    (Case (Cast (Var "x") TDyn (TName False "Bool")) [("'T",Math (Add (Lit (LNat 17)) (Var "y"))),("'F",App (Var "not") (Var "y"))]))

-- type Direction : ~un = {'L, 'R}
directionType = DType "Direction" MMany Kun (TLab ["'L","'R"])
directionTypeVal = ("Direction", VType $ TLab ["'L","'R"])

-- val f3 = 𝜆(x: Bool) 𝜆(y: *) 𝜆(z: case (y: * => case x {'T: Direction, 'F: Bool}) {'T: Direction, 'F: Bool, 'L: Bool, 'R: Bool})
--            case (y: * => case x {'T: Direction, 'F: Bool}) {'T: y, 'F: not y, 'L: z, 'R: not z}
f3 = Lam MMany "x" (TName False "Bool")
  (Lam MMany "y" TDyn
    (Lam MMany "z"
      (TCase (Cast (Var "y") TDyn (TCase (Var "x") [("'T",TName False "Direction"),("'F",TName False "Bool")]))
        [("'T",TName False "Direction"),("'F",TName False "Bool"),("'L",TName False "Bool"),("'R",TName False "Bool")])
        (Case (Cast (Var "y") TDyn (TCase (Var "x") [("'T",TName False "Direction"),("'F",TName False "Bool")]))
          [("'T",Var "y"),("'F",App (Var "not") (Var "y")),("'L",Var "z"),("'R",App (Var "not") (Var "z"))])))

-- val f4' = 𝜆(x: Bool) 𝜆(y: *) 𝜆(z: Bool) case x {'T: (y: * => (a:Bool) -> (b:Bool) -> Bool) z z, 'F: (y: * => (b:Bool) -> Bool) z}
f4' = Lam MMany "x" (TName False "Bool")
  (Lam MMany "y" TDyn
    (Lam MMany "z" (TName False "Bool")
      (Case (Var "x")
        [("'T",App (App (Cast (Var "y") TDyn (TFun MMany "a" (TName False "Bool") (TFun MMany "b" (TName False "Bool") (TName False "Bool")))) (Var "z")) (Var "z"))
        ,("'F",App (Cast (Var "y") TDyn (TFun MMany "b" (TName False "Bool") (TName False "Bool"))) (Var "z"))])))

-- val f4 = 𝜆(x: Bool) 𝜆(y: *) case x {'T: (y: * => (a:Bool) -> (b:Bool) -> Bool) x x, 'F: (y: * => (a:Bool) -> Bool) x}
f4 = Lam MMany "x" (TName False "Bool")
  (Lam MMany "y" TDyn
    (Case (Var "x")
      [("'T",App (App (Cast (Var "y") TDyn (TFun MMany "a" (TName False "Bool") (TFun MMany "b" (TName False "Bool") (TName False "Bool")))) (Var "x")) (Var "x"))
      ,("'F",App (Cast (Var "y") TDyn (TFun MMany "a" (TName False "Bool") (TName False "Bool"))) (Var "x"))]))

-- rec f ( x . (fn (acc : Int) fn (x : Int) f (acc + x)) ) (fn (acc : Int) acc)
sumfRec = Rec "f" "n1"
  (Lam MMany "acc" TInt
    (Lam MMany "x" TInt
      (App (App (Var "f") (Var "n1")) (Math (Add (Var "acc") (Var "x"))))))
  (Lam MMany "acc" TInt (Var "acc"))

-- new_natrec f : n . A . (acc : Int) -> A
--   { fn (acc: Int) acc
--   , n1. fn (acc : Int) fn (x : Int) f n1 (acc + x) }
newsumfRec = NewNatRec
  "f" "n1" "A" (TFun MMany "acc" TInt (TName False "A"))
  (Lam MMany "acc" TInt (Var "acc"))
  "n1"
  (Lam MMany "acc" TInt
    (Lam MMany "x" TInt
      (App
        (App (Var "f") (Var "n1"))
        (Math (Add (Var "acc") (Var "x"))))))
