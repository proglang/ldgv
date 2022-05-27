module TypecheckerSpec (spec) where

import Test.Hspec
import Typechecker
import UtilsFuncCcldlc
import Syntax
import Kinds

tcOptionsCast :: Options
tcOptionsCast = Options{ gradual = False }

spec :: Spec
spec = do
  describe "CCLDLC specific typechecking" $ do
    it "typechecks example function f1'" $ do
      let f = DFun "f" [] f1' Nothing
      typecheck tcOptionsCast [boolType, notFunc, f] `shouldBe` Right ()
    it "typechecks example function f2'" $ do
      let f = DFun "f" [] f2' Nothing
      typecheck tcOptionsCast [boolType, notFunc, f] `shouldBe` Right ()
    it "typechecks example function f" $ do
      let f' = DFun "f" [] f Nothing
      typecheck tcOptionsCast [boolType, notFunc, f'] `shouldBe` Right ()
    it "typechecks example function f3" $ do
      let f = DFun "f" [] f3 Nothing
      typecheck tcOptionsCast [boolType, notFunc, directionType, f] `shouldBe` Right ()
    it "typechecks example function f4'" $ do
      let f = DFun "f" [] f4' Nothing
      typecheck tcOptionsCast [boolType, notFunc, f] `shouldBe` Right ()
    it "typechecks example function f4" $ do
      let f = DFun "f" [] f4 Nothing
      typecheck tcOptionsCast [boolType, notFunc, f] `shouldBe` Right ()
    it "typechecks section 5.1 (2) function, expecting failure" $ do
      let term = DSub (TFun MMany "x" TUnit TInt) (TFun MMany "x" TDyn (TCase (Cast (Var "x") TDyn (TName False "Bool")) [("'T",TInt),("'F",TName False "Bool")]))
      typecheck tcOptionsCast [boolType, term] `shouldBe` Left "Subtyping fails to establish Int <: {'T, 'F}"
