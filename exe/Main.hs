{-# LANGUAGE ApplicativeDo #-}
module Main where

import Control.Applicative
import Control.Monad
import qualified Options.Applicative as Opts

actionParser :: Opts.Parser (IO ())
actionParser = Opts.hsubparser $ mconcat
  [ Opts.command "compile"
      $ Opts.info compileParser
      $ Opts.progDesc "Parse, typecheck and transpile an LDGV/LDST program to C."
  , Opts.command "interpret"
      $ Opts.info interpretParser
      $ Opts.progDesc "Parse, typecheck and interpret an LDGV/LDST program."
  ]
  where
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
  [ Opts.progDesc "An implementation of the LDGV/LDST language."
  , Opts.footer "Authors: \
      \Nils Hagner (interpreter, web frontend), \
      \Janek Spaderna (C backend, command line frontend), \
      \Peter Thiemann (parser, typechecker)"
  ]

main :: IO ()
main = join $ Opts.customExecParser prefs actionParserInfo
  where
    prefs = Opts.prefs Opts.showHelpOnEmpty

interpret :: Maybe FilePath -> IO ()
interpret _minput = do
  fail "interpret: not yet hooked up"

compile :: Maybe FilePath -> Maybe FilePath -> IO ()
compile _minput _moutput = do
  fail "compile: not yet implemented"
