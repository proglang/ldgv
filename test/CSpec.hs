{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
module CSpec (spec) where

import Control.Monad.Reader
import Data.ByteString.Builder (hPutBuilder)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (decodeUtf8)
import System.Exit
import System.FilePath
import System.IO
import System.Process.Typed
import Test.Hspec
import UnliftIO.Temporary
import Utils
import qualified Data.Text.Lazy as TL

import C.Compile
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

  describe "error conditions" do
    it "detects simple deadlocks" do
      let src = unlines
            [ "val main : Int"
            , "val main ="
            , "  let <aS, aR> = new !Int.Unit in"
            , "  let <bS, bR> = new !Int.Unit in"
            , "  let x = fork (send aS (fst (recv bR))) in"
            , "  send bS (fst (recv aR))"
            ]
      src `shouldEvaluateTo` Left "deadlocked"


shouldEvaluateTo :: HasCallStack => String -> Either Text Text -> Expectation
shouldEvaluateTo source result = do
  let ast = parseDecls source
  let (exitPred, result') = case result of
        Left r -> (shouldNotBe, "error: " <> r)
        Right r -> (shouldBe, "result: " <> r)

  code <- case ast >>= generate (Just "main") of
    Left err -> raiseFailure $ "cannot generate code:\n" ++ err
    Right code -> pure code

  withSystemTempDirectory "compiled" \dirpath -> do
    let codePath = dirpath </> "source.c"
    let outPath = dirpath </> "out"

    -- Write the code
    withBinaryFile codePath WriteMode \h ->
      hPutBuilder h code

    -- Link the program
    runReaderT (link outPath [codePath] "serial") defaultEnv

    -- Execute it, capturing the output
    (exitCode, output) <- readProcessInterleaved $ proc outPath []
    exitCode `exitPred` ExitSuccess
    TL.strip (decodeUtf8 output) `shouldBe` result'
