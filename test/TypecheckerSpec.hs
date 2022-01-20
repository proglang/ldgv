module TypecheckerSpec (spec) where

import Test.Hspec
import Typechecker
import UtilsFuncCcldlc
import Syntax

spec :: Spec
spec = do
  describe "CCLDLC specific typechecking" $ do
    it "typechecks example function f1'" $ do
      let f = DFun "f" [] f1' Nothing
      typecheck [boolType, notFunc, f] `shouldBe` Right ()
    it "typechecks example function f2'" $ do
      let f = DFun "f" [] f2' Nothing
      typecheck [boolType, notFunc, f] `shouldBe` Right ()
    it "typechecks example function f" $ do
      let f' = DFun "f" [] f Nothing
      typecheck [boolType, notFunc, f'] `shouldBe` Right ()
    it "typechecks example function f3" $ do
      let f = DFun "f" [] f3 Nothing
      typecheck [boolType, notFunc, directionType, f] `shouldBe` Right ()
    it "typechecks example function f4'" $ do
      let f = DFun "f" [] f4' Nothing
      typecheck [boolType, notFunc, f] `shouldBe` Right ()
