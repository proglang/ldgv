{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Data.ByteString.Builder
import Data.Foldable
import Data.Maybe
import System.Exit
import System.IO
import UnliftIO.Temporary
import qualified Options.Applicative as Opts

import Kinds
import Parsing
import qualified C.Compile as C
import qualified C.Generate as C
import qualified Interpreter as I
import qualified ProcessEnvironment as P
import qualified Syntax
import qualified Typechecker as T

data CompileMode
  = CompileC
  | CompileObject
  | CompileLink String
  deriving (Show, Eq) 

data CompileOpts = Compile
  { compileInput    :: !(Maybe FilePath)
  , compileOutput   :: !(Maybe FilePath)
  , compileMainId   :: !(Maybe Syntax.Ident)
  , compileMainSig  :: !(Maybe Syntax.Type)
  , compileMode     :: !CompileMode
  , compileEnv      :: !C.Env
  }
  deriving (Show)

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
      compileMainId <- optional $ Opts.strOption $ mconcat
        [ Opts.long "main"
        , Opts.short 'm'
        , Opts.metavar "DECL"
        , Opts.help "Generate a ‘main()’ function, it evaluates the given \
            \declaration which must have a type signature and have no \
            \parameters to do something useful."
        ]
      compileMainSig <- optional $ Opts.option (Opts.eitherReader parseType) $ mconcat
        [ Opts.long "main-sig"
        , Opts.short 's'
        , Opts.metavar "TYPE"
        , Opts.help "Provides a type signature for the --main declaration \
            \without having to edit the source code."
        ]
      compileOutput <- optional $ Opts.strOption $ mconcat
        [ Opts.long "output"
        , Opts.short 'o'
        , Opts.metavar "FILE"
        , Opts.help "Write the result to FILE, this defaults to STDOUT in -C mode."
        ]
      compileMode <- pure CompileC <|> asum
        [ Opts.flag' CompileC $ mconcat
            [ Opts.short 'C'
            , Opts.help "Generate C code, this is the default if none of -O or -L is given."
            ]
        , Opts.flag' CompileObject $ mconcat
            [ Opts.short 'O'
            , Opts.help "Generate C code and compile to an object file."
            ]
        , fmap CompileLink $ Opts.strOption $ mconcat
            [ Opts.short 'L'
            , Opts.long "link"
            , Opts.metavar "BACKEND"
            , Opts.help "Generate C code and link to an executable with BACKEND."
            ]
        ]
      compileEnv <- do
        cc <- Opts.strOption $ mconcat
          [ Opts.long "cc"
          , Opts.value "cc"
          , Opts.showDefault
          , Opts.metavar "CC"
          , Opts.help "Use CC to compile and link programs in modes -O and -L."
          ]
        opts <- many $ Opts.strOption $ mconcat
          [ Opts.long "cc-option"
          , Opts.metavar "OPT"
          , Opts.help "Additional option passed to CC. \
              \This option can be given multiple times."
          ]
        opts' <- many $ Opts.strOption $ mconcat
          [ Opts.long "cc-options"
          , Opts.metavar "OPTS"
          , Opts.help "Additional options passed to CC, split on whitespace. \
              \This option can be given multiple times."
          ]
        pure C.Env{ envCC = cc, envFlags = opts ++ concatMap words opts', envVerbose = True }
      compileInput <- optional inPathArg
      pure $ compile Compile{..}

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

compile :: CompileOpts -> IO ()
compile co = do
  mbackend <- case compileMode co of
    CompileC -> do
      generate co \b -> withOutput (compileOutput co) (`hPutBuilder` b)
      exitSuccess
    CompileObject -> pure Nothing
    CompileLink backend -> pure (Just backend)

  outputFP <- maybe
    (msgError "--output must be given for modes -O and -L.")
    pure
    (compileOutput co)

  -- The generated C code is written to a temporary file and then compiled/linked.
  withSystemTempFile (fromMaybe "code" (compileInput co) ++ ".c") \codeFP codeHandle -> do
    generate co (hPutBuilder codeHandle)
    hClose codeHandle

    let invocation = maybe
          (C.compile outputFP codeFP)
          (C.link outputFP [codeFP])
          mbackend
    runReaderT invocation (compileEnv co)

generate :: CompileOpts -> (Builder -> IO ()) -> IO ()
generate Compile{..} writeOutput = do
  when (isNothing compileMainId && isJust compileMainSig) do
    msgWarning "--main-sig is not used because no --main is given."

  let addSig' (ident, typ) = (Syntax.DSig ident Many typ :)
  let addSig = maybe id addSig' ((,) <$> compileMainId <*> compileMainSig)
  decls <- addSig <$> parseInput compileInput
  either msgError writeOutput (T.typecheck decls >> C.generate compileMainId decls)
  

parseInput :: Maybe FilePath -> IO [Syntax.Decl]
parseInput = maybe getContents readFile >=> either msgError pure . parseDecls

withOutput :: Maybe FilePath -> (Handle -> IO r) -> IO r
withOutput = maybe ($ stdout) (\fp -> withBinaryFile fp WriteMode)

msgWarning :: String -> IO ()
msgWarning msg = hPutStrLn stderr ("warning: " ++ msg)

msgError :: String -> IO a
msgError msg = hPutStrLn stderr ("error: " ++ msg) >> exitFailure
