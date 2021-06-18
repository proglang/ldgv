{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Main (main) where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Data.ByteString.Builder
import Data.Foldable
import Data.Maybe
import Data.Monoid
import System.Exit
import System.FilePath
import UnliftIO
import qualified Options.Applicative as Opts

import Kinds
import Output
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
  { compileInputs   :: ![FilePath]
  , compileOutput   :: !(Maybe FilePath)
  , compileMainId   :: !(Maybe Syntax.Ident)
  , compileMainSig  :: !(Maybe Syntax.Type)
  , compileMode     :: !CompileMode
  , compileEnv      :: !C.Env
  }
  deriving stock (Show)

actionParser :: Opts.Parser (Action ())
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

    typecheckParser =
      typecheck <$> inPathArgs

    interpretParser =
      interpret <$> inPathArgs

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
      compileInputs <- inPathArgs
      pure $ compile Compile{..}

    inPathArgs :: Opts.Parser [FilePath]
    inPathArgs = many $ Opts.strArgument $ mconcat
        [ Opts.metavar "SRC-FILES"
        , Opts.help "Read the input from SRC-FILES, uses STDIN if no path is given."
        ]

actionParserInfo :: Opts.ParserInfo (Action ())
actionParserInfo = Opts.info (actionParser <**> Opts.helper) $ mconcat
  [ Opts.progDesc "An implementation of Label Dependent Session Types (LDST)."
  , Opts.footer "Authors: \
      \Nils Hagner (interpreter, web frontend), \
      \Janek Spaderna (C backend, command line frontend), \
      \Peter Thiemann (parser, typechecker)"
  ]

main :: IO ()
main = do
  let prefs = Opts.prefs Opts.showHelpOnEmpty
  action <- Opts.customExecParser prefs actionParserInfo
  runTerminalSize action

typecheck :: [FilePath] -> Action ()
typecheck inputs = do
  decls <- parseInput inputs
  liftIO case T.typecheck decls of
    Right _ -> putStrLn "Good!"
    Left err -> putStrLn $ "Error: " ++ err

interpret :: [FilePath] -> Action ()
interpret inputs = do
  res <- try $ do
    decls <- parseInput inputs
    case T.typecheck decls of
      Right a -> pure a
      Left err -> fail $ "Error: " ++ err
    liftIO $ I.interpret decls
  liftIO $ putStrLn $ either
    (\v -> "Error: " ++ show v)
    (\v -> "Result: " ++ show v)
    (res :: Either SomeException P.Value)

compile :: CompileOpts -> Action ()
compile co = do
  mbackend <- case compileMode co of
    CompileC -> do
      generate co \b -> withOutput (compileOutput co) (`hPutBuilder` b)
      liftIO exitSuccess
    CompileObject -> pure Nothing
    CompileLink backend -> pure (Just backend)

  outputFP <- maybe
    (msgFatal "--output must be given for modes -O and -L.")
    pure
    (compileOutput co)

  -- The generated C code is written to a temporary file and then compiled/linked.
  let temporaryCCodeName = takeFileName outputFP  <.> ".c"
  withSystemTempFile temporaryCCodeName \codeFP codeHandle -> do
    generate co (hPutBuilder codeHandle)
    hClose codeHandle

    let invocation = maybe
          (C.compile outputFP codeFP)
          (C.link outputFP [codeFP])
          mbackend
    runReaderT invocation (compileEnv co)

generate :: CompileOpts -> (Builder -> IO ()) -> Action ()
generate Compile{..} writeOutput = do
  when (isNothing compileMainId && isJust compileMainSig) do
    msgWarning "--main-sig is not used because no --main is given."

  let addSig' (ident, typ) = (Syntax.DSig ident Many typ :)
  let addSig = maybe id addSig' ((,) <$> compileMainId <*> compileMainSig)
  decls <- addSig <$> parseInput compileInputs
  either msgFatal (liftIO . writeOutput) (T.typecheck decls >> C.generate compileMainId decls)
  
parseInput :: [FilePath] -> Action [Syntax.Decl]
parseInput fps = do
  -- Unwraps a parse result of type @Maybe [Decl]@.
  let unwrap = liftIO . maybe exitFailure pure

  -- Like 'unwrap' above but combines multiple parse results before unwrapping.
  --
  -- Using the `Ap` wrapper the many parse results are turned into one failed
  -- result should at least one parse have failed.
  let unwrapMany = unwrap . getAp . foldMap Ap

  if null fps
     then parseFile Nothing >>= unwrap
     else mapM (parseFile . Just) fps >>= unwrapMany

parseFile :: Maybe FilePath -> Action (Maybe [Syntax.Decl])
parseFile mpath = do
  (name, src) <- liftIO $ case mpath of
    Nothing -> ("<stdin>",) <$> getContents
    Just fp -> (fp,) <$> readFile fp

  case parseDecls src of
    Left err -> Nothing <$ formatMsg MsgError (Just name) err
    Right decls -> pure (Just decls)


withOutput :: MonadUnliftIO m => Maybe FilePath -> (Handle -> m r) -> m r
withOutput = maybe ($ stdout) (\fp -> withBinaryFile fp WriteMode)
