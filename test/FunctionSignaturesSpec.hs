{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall #-}
module FunctionSignaturesSpec (spec) where

import Test.Hspec

import Kinds
import MonadOut
import Syntax
import Typechecker

spec :: Spec
spec = do
  let tcShouldFail :: [Decl] -> Expectation
      tcShouldFail decls =
        runOutIgnore (typecheck decls) `shouldThrow` anyIOException

  let tcShouldSucceed :: [Decl] -> Expectation
      tcShouldSucceed decls =
        runOutIgnore (typecheck decls)

  describe "duplicate signatures" do
    it "raises an error if the types agree" do
      tcShouldFail 
        [ DSig "main" Many TInt
        , DSig "main" Many TInt
        ]

    it "raises an error if the types disagree" do
      tcShouldFail
        [ DSig "main" Many TInt
        , DSig "main" Many TUnit
        ]

  describe "duplicate definitions" do
    it "raises an error if the types agree" do
      let decls a r = replicate 2 $ DFun "main" a Unit r
      tcShouldFail $ decls [] Nothing
      tcShouldFail $ decls [] (Just TUnit)
      tcShouldFail $ decls [(MMany, "a", TInt)] Nothing
      tcShouldFail $ decls [(MMany, "a", TInt)] (Just TUnit)

    it "raises an error if the types disagree" do
      tcShouldFail
        [ DFun "main" [] Unit Nothing
        , DFun "main" [(MMany, "a", TInt)] Unit Nothing
        ]

  describe "signature and definition" do
    describe "agreeing types" do
      it "typechecks to give a signature but no return type" do
        tcShouldSucceed
          [ DSig "main" Many $ TFun MMany "a" TInt TUnit
          , DFun "main" [(MMany, "a", TInt)] Unit Nothing
          ]
      it "typechecks to give a signature and return type" do
        tcShouldSucceed
          [ DSig "main" Many $ TFun MMany "a" TInt TUnit
          , DFun "main" [(MMany, "a", TInt)] Unit (Just TUnit)
          ]
      it "typechecks if the parameter names differ" do
        tcShouldSucceed
          [ DSig "main" Many $ TFun MMany "a" TInt TUnit
          , DFun "main" [(MMany, "b", TInt)] Unit Nothing
          ]

    describe "disagreeing types" do
      it "raises an error if the types don't match" do
        tcShouldFail
          [ DSig "main" Many TInt
          , DFun "main" [] Unit Nothing
          ]
      it "raises an error if the returns type doesn't match" do
        -- The function definition would be alright by itself but the return
        -- type has to be equivalent with the given signature.
        tcShouldFail
          [ DSig "main" Many $ TLab ["'A", "'B"]
          , DFun "main" [] (Lab "'A") (Just $ TLab ["'A"])
          ]
