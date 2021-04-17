{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}
module CSpec (spec) where

import Control.Monad.Reader
import Data.ByteString.Builder (hPutBuilder)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (decodeUtf8)
import System.Exit
import System.FilePath
import System.Process.Typed
import Test.Hspec
import UnliftIO
import Utils
import qualified Data.Text.Lazy as TL

import C.Compile as C
import C.Generate
import Parsing

spec :: Spec
spec = parallel do
  describe "simple returns" do
    it "prints numbers" do
      let src = unlines
            [ "val main : Int"
            , "val main = 42"
            ]
      src `shouldEvaluateTo` Right "Int 42"

    it "prints labels" do
      let src = unlines
            [ "val main : { 'A, 'B }"
            , "val main = 'A"
            ]
      src `shouldEvaluateTo` Right "Label 'A"

    it "prints pairs" do
      let src = unlines
            [ "val main : [ Int, { 'A, 'B } ]"
            , "val main = < n = 42, 'B >"
            ]
      src `shouldEvaluateTo` Right "<Int 42, Label 'B>"

  -- This tests for miscompilation of shadowed variables.
  describe "name shadowing" do
    context "in the source language" do
      it "shadows only locally" do
        let src = unlines
              [ "val add (a : Int) (b : Int) = a + b"
              , "val main : Int"
              , "val main = "
              , "  let x = 10 in"
              , "  (let x = 20 in add x) x"
              ]
        src `shouldEvaluateTo` Right "Int 30"

      it "pair construction" do
        let src = unlines
              [ "val main : [ Int, [ Int, [ Int, Int ] ] ]"
              , "val main = < n = 1, < n = 2, < n = 3, 4 > > >"
              ]
        src `shouldEvaluateTo` Right "<Int 1, <Int 2, <Int 3, Int 4>>>"

      it "pair access with fst/snd" do
        let src = unlines
              [ "val main : [ {'A}, Int ]"
              , "val main = "
              , "  let letpair = 'A in "
              , "  let x = snd <x=10, 20> in "
              , "  <foo = letpair, x>"
              ]
        src `shouldEvaluateTo` Right "<Label 'A, Int 20>"

      it "pair destructuring" do
        let src = unlines
              [ "val add (a : Int) (b : Int) = a + b"
              , "val main : Int"
              , "val main = "
              , "  let x = 10 in"
              , "  let p = < a = 20, 30 > in"
              , "  (let <x, y> = p in add x) x" 
              ]
        src `shouldEvaluateTo` Right "Int 30"

    context "in the target language" do
      it "escapes parameters looking like stdlib functions" do
        let src = unlines
              [ -- Typechecking fails without type signature for `foo`.
                "val foo : (malloc : Int) -> [ Int, Int ]"
              , "val foo (malloc : Int) = < x = malloc, malloc >"
              , "val main : [ Int, Int ]"
              , "val main = foo 20"
              ]
        src `shouldEvaluateTo` Right "<Int 20, Int 20>"

      it "escapes parameters looking like generated local variables" do
        let src = unlines
              [ -- Typechecking fails without type signature for `foo`.
                "val foo : (foo_0 : Int) -> [ Int, Int ]"
              , "val foo (foo_0 : Int) = < x = foo_0, foo_0 >"
              , "val main : [ Int, Int ]"
              , "val main = foo 20"
              ]
        src `shouldEvaluateTo` Right "<Int 20, Int 20>"

  -- This tests for a miscompilation in the closure sharing algorithm.
  describe "closure sharing" do
    it "keeps skipped arguments" do
      let src = unlines
            [ "val add (a : Int) (b : Int) (c : Int) = a + b + c"
            , "val main : Int"
            , "val main = "
            , "  let a = 10 in"
            , "  let b = 20 in"
            , "  let calc = fn (x : Unit) add a 50 b in"
            , "  calc ()"
            ]
      src `shouldEvaluateTo` Right "Int 80"

  describe "error conditions" do
    it "detects simple deadlocks" do
      let src = unlines
            [ "val main : Unit"
            , "val main ="
            , "  let <aS, aR> = new !Int.Unit in"
            , "  let <bS, bR> = new !Int.Unit in"
            , "  let x = fork (send aS (fst (recv bR))) in"
            , "  send bS (fst (recv aR))"
            ]
      src `shouldEvaluateTo` Left "deadlocked"

shouldEvaluateTo :: HasCallStack => String -> Either Text Text -> Expectation
shouldEvaluateTo source result = do
  let (exitPred, result') = case result of
        Left r -> (shouldNotBe, "error: " <> r)
        Right r -> (shouldBe, "result: " <> r)

  withGeneratedCode source \tmpDir codePath -> do
    -- Link the program
    let outPath = tmpDir </> "a.out"
    let flags = "-Werror" : envFlags defaultEnv
    let env = defaultEnv { envFlags = flags }
    runReaderT (C.link outPath [codePath] "serial") env

    -- Execute it, capturing the output
    (exitCode, output) <- readProcessInterleaved $ proc outPath []
    exitCode `exitPred` ExitSuccess
    TL.strip (decodeUtf8 output) `shouldBe` result'

withGeneratedCode
  :: HasCallStack
  => String
  -> (HasCallStack => FilePath -> FilePath -> IO r)
  -> IO r
withGeneratedCode source body = do
  let parseAndCheck = do
        decls <- parseDecls source
        typecheck decls
        pure decls
  code <- case parseAndCheck >>= generate (Just "main") of
    Left err -> raiseFailure $ "cannot generate code:\n" ++ err
    Right code -> pure code

  -- Write code to a file in a temporary directory
  withSystemTempDirectory "compiled" \dirpath -> do
    let codePath = dirpath </> "source.c"
    withBinaryFile codePath WriteMode \h ->
      hPutBuilder h code
    body dirpath codePath
