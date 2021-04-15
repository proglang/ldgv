{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Output where

import Control.Monad.Reader
import Prettyprinter
import Prettyprinter.Util (reflow)
import System.Exit
import System.IO
import qualified Data.Text as Text
import qualified Prettyprinter.Render.Terminal as Terminal
import qualified System.Console.Terminal.Size as Terminal

newtype TerminalSize = TerminalSize (Maybe Int)

type Action = ReaderT TerminalSize IO

runTerminalSize :: Action a -> IO a
runTerminalSize action = do
  !mcols <- stderrColumns
  runReaderT action (TerminalSize mcols)

-- | Tries to determine the size of the terminal attached to stderr using the
-- terminal-size package.
--
-- On windows only @"System.Console.Terminal.Size".'Terminal.size'@ is
-- available, this isn't suitable for the non-windows case: if stdout is
-- redirected but stderr goes to the terminal it will return @Nothing@ instead
-- of the actual terminal dimensions.
stderrColumns :: MonadIO m => m (Maybe Int)
stderrColumns = liftIO $ fmap Terminal.width <$!> size
  where
    size =
#if defined(mingw32_HOST_OS)
      Terminal.size
#else
      Terminal.hSize stderr
#endif


data MsgLevel
  = MsgError
  | MsgWarning

msgWarning :: (MonadReader TerminalSize m, MonadIO m) => String -> m ()
msgWarning = formatMsg MsgWarning Nothing

msgFatal :: (MonadReader TerminalSize m, MonadIO m) => String -> m a
msgFatal msg = formatMsg MsgError Nothing msg >> liftIO exitFailure

formatMsg :: (MonadIO m, MonadReader TerminalSize m) => MsgLevel -> Maybe FilePath -> String -> m ()
formatMsg level mfile msg = do
  let levelDoc =
        let (levelString, color) = case level of
              MsgError -> ("error", Terminal.Red)
              MsgWarning -> ("warning", Terminal.Yellow)
         in annotate (Terminal.color color) $ levelString <> ": "

  let fileDoc = flip foldMap mfile \file ->
        pretty file <> ": "

  let doc = mconcat
        [ annotate Terminal.bold $ fileDoc <> levelDoc
        , nest 4 $ reflow $ Text.pack msg
        ]

  TerminalSize mcols <- ask
  let prettyWidth = maybe Unbounded (\cols -> AvailablePerLine (min cols 80) 0.9) mcols
  let prettyOpts = defaultLayoutOptions { layoutPageWidth = prettyWidth }
  liftIO $ Terminal.renderIO stderr $ layoutPretty prettyOpts doc
  liftIO $ hPutChar stderr '\n'
