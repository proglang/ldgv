{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
module Main (main) where

import Control.Applicative
import Control.Category ((>>>))
import Control.Exception
import Control.Monad
import Data.ByteString.Builder
import System.IO
import qualified Options.Applicative as Opts

import MonadOut
import Parsing
import Typechecker
import qualified Interpreter as I
import qualified ProcessEnvironment as P
import qualified Syntax
import qualified Target.C as C

actionParser :: Opts.Parser (IO ())
actionParser = do
  debug <- Opts.switch $ mconcat
    [ Opts.long "debug"
    , Opts.help "Display debug output"
    ]
  cmd <- commands
  pure $ cmd debug
  where
    commands = Opts.hsubparser $ mconcat
      [ Opts.command "compile"
          $ Opts.info compileParser
          $ Opts.progDesc "Parse, typecheck and transpile an LDGV/LDST program to C."
      , Opts.command "interpret"
          $ Opts.info interpretParser
          $ Opts.progDesc "Parse, typecheck and interpret an LDGV/LDST program."
      ]

    interpretParser = do
      inPath <- optional inPathArg
      pure $ interpret inPath

    compileParser = do
      outPath <- optional $ Opts.strOption $ mconcat
        [ Opts.long "output"
        , Opts.short 'o'
        , Opts.metavar "FILE"
        , Opts.help "Write the generated C code to FILE or to STDOUT if not given."
        ]
      inPath <- optional inPathArg
      pure $ compile inPath outPath

    inPathArg = Opts.strArgument $ mconcat
        [ Opts.metavar "SRC-FILE"
        , Opts.help "Read the input from FILE or from STDIN if no argument is given."
        ]

actionParserInfo :: Opts.ParserInfo (IO ())
actionParserInfo = Opts.info (actionParser <**> Opts.helper) $ mconcat
  [ Opts.progDesc "An implementation of Label Dependent Session Types (LDST)."
  , Opts.footer "Authors: \
      \Nils Hagner (interpreter, web frontend), \
      \Janek Spaderna (C backend, command line frontend), \
      \Peter Thiemann (parser, typechecker)"
  ]

main :: IO ()
main = join $ Opts.customExecParser prefs actionParserInfo
  where
    prefs = Opts.prefs Opts.showHelpOnEmpty

interpret :: Maybe FilePath -> Bool -> IO ()
interpret minput debug = do
  res <- try $ do
    decls <- parseInput minput
    if debug
       then runOutIO $ typecheck decls
       else runOutIgnore $ typecheck decls
    I.interpret decls
  putStrLn $ either
    (\v -> "Error: " ++ show v)
    (\v -> "Result: " ++ show v)
    (res :: Either SomeException P.Value)

compile :: Maybe FilePath -> Maybe FilePath -> Bool -> IO ()
compile minput moutput debug = do
  decls <- parseInput minput
  if debug
     then runOutIO $ typecheck decls
     else runOutIgnore $ typecheck decls
  withOutput moutput $ \outHandle ->
    hPutBuilder outHandle (C.generate decls)

parseInput :: Maybe FilePath -> IO [Syntax.Decl]
parseInput = maybe getContents readFile >>> fmap parse

withOutput :: Maybe FilePath -> (Handle -> IO r) -> IO r
withOutput = maybe ($ stdout) (\fp -> withBinaryFile fp WriteMode)
