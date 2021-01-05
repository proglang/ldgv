{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module BrowserOut where

import Control.Monad
import Control.Monad.Fail
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import qualified Data.Text as T
import Language.Javascript.JSaddle
import MonadOut

newtype BrowserOutT m a = BrowserOutT { runBrowserOutT :: m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadFail)

instance MonadTrans BrowserOutT where
  lift = BrowserOutT

instance MonadOut (BrowserOutT JSM) where
  output s = BrowserOutT $ do
    let t' = T.replace "\n" "\\n" $ T.pack s
    let t = T.replace "'" "\\'" t'
    void $ eval $ T.concat ["document.getElementById('tOutput').value += '", t, "\\n'"]
