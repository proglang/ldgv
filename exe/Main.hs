{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Main (main) where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.ByteString.Builder
import System.IO
import qualified Options.Applicative as Opts

import Data.Maybe
import Kinds
import Parsing
import System.Exit
import qualified C.Generate as C
import qualified Interpreter as I
import qualified ProcessEnvironment as P
import qualified Syntax
import qualified Typechecker as T

actionParser :: Opts.Parser (IO ())
actionParser = commands
  where
    commands = Opts.hsubparser $ mconcat
      [ Opts.command "compile"
          $ Opts.info compileParser
          $ Opts.progDesc "Parse, typecheck and transpile an LDGV/LDST program to C."
      , Opts.command "interpret"
          $ Opts.info interpretParser
          $ Opts.progDesc "Parse, typecheck and interpret an LDGV/LDST program."
      , Opts.command "typecheck"
          $ Opts.info typecheckParser
          $ Opts.progDesc "Only typecheck an LDGV/LDST program."
      ]

    interpretParser = do
      inPath <- optional inPathArg
      pure $ interpret inPath

    compileParser = do
      mainCall <- optional $ Opts.strOption $ mconcat
        [ Opts.long "main"
        , Opts.short 'm'
        , Opts.metavar "DECL"
        , Opts.help "Generate a ‘main()’ function, it evaluates the given \
            \declaration which must have a type signature and have no \
            \parameters to do something useful."
        ]
      mainSig <- optional $ Opts.option (Opts.eitherReader parseType) $ mconcat
        [ Opts.long "main-sig"
        , Opts.short 's'
        , Opts.metavar "TYPE"
        , Opts.help "Provides a type signature for the --main declaration \
            \without having to edit the source code."
        ]
      outPath <- optional $ Opts.strOption $ mconcat
        [ Opts.long "output"
        , Opts.short 'o'
        , Opts.metavar "FILE"
        , Opts.help "Write the generated C code to FILE or to STDOUT if not given."
        ]
      inPath <- optional inPathArg
      pure $ compile inPath outPath mainCall mainSig

    typecheckParser = do
      inPath <- optional inPathArg
      pure $ typecheck inPath

    inPathArg = Opts.strArgument $ mconcat
        [ Opts.metavar "SRC-FILE"
        , Opts.help "Read the input from FILE, uses STDIN if no argument is given."
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

typecheck :: Maybe FilePath -> IO ()
typecheck minput = do
  decls <- parseInput minput
  case  T.typecheck decls of
    Right _ -> putStrLn "Good!"
    Left err -> putStrLn $ "Error: " ++ err

interpret :: Maybe FilePath -> IO ()
interpret minput = do
  res <- try $ do
    decls <- parseInput minput
    case T.typecheck decls of
      Right a -> pure a
      Left err -> fail $ "Error: " ++ err
    I.interpret decls
  putStrLn $ either
    (\v -> "Error: " ++ show v)
    (\v -> "Result: " ++ show v)
    (res :: Either SomeException P.Value)

compile :: Maybe FilePath -> Maybe FilePath -> Maybe String -> Maybe Syntax.Type -> IO ()
compile minput moutput mmainIdent mmainType = do
  when (isNothing mmainIdent && isJust mmainType) do
    hPutStrLn stderr "warning: --main-sig is not used because no --main is given."

  let addSig' (ident, typ) = (Syntax.DSig ident Many typ :)
  let addSig = maybe id addSig' ((,) <$> mmainIdent <*> mmainType)
  decls <- addSig <$> parseInput minput

  case T.typecheck decls >> C.generate mmainIdent decls of
    Left err -> do
      hPutStrLn stderr err
      exitFailure
    Right code ->
      withOutput moutput $ \outHandle ->
        hPutBuilder outHandle code

parseInput :: Maybe FilePath -> IO [Syntax.Decl]
parseInput = maybe getContents readFile >=> either fail pure . parseDecls

withOutput :: Maybe FilePath -> (Handle -> IO r) -> IO r
withOutput = maybe ($ stdout) (\fp -> withBinaryFile fp WriteMode)
