{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Data.String as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Map as Map
import Reflex.Dom.Core
import Language.Javascript.JSaddle
import Language.Javascript.JSaddle (eval, liftJSM)

import Config as C (putStrLn)
import Interpreter as I
import Typechecker (typecheck)
import qualified Examples as E
import Control.Exception (try, SomeException)
import ProcessEnvironment as P (Value) 

widgetHead :: DomBuilder t m => m ()
widgetHead = do
  el "title" $ text "LDGV Interpreter"
  elAttr "link" ("rel" =: "stylesheet" <> "href" =: "main.css" <> "type" =: "text/css") blank

-- read the text inside our textarea
textareaget :: JSM T.Text
textareaget = valToText $ eval ("document.getElementById('tSrc').value" :: T.Text)

-- set the text inside an element given by id
setHtmlElement :: String -> String -> JSM ()
setHtmlElement id s = do
    val <- eval (T.pack $ "document.getElementById('" ++ id ++ "').value = " ++ (show s))
    pure ()

setSrc = setHtmlElement "tSrc"
setRes = setHtmlElement "tResult"
resetOutput = setHtmlElement "tOutput" ""

main = mainWidgetWithHead widgetHead $ el "div" $ do
    let srcfiles = E.filenames
    -- create a map for lookup of all texts (by dropdown value)
    let exampleFilesMap = Map.fromList [(name, T.pack name) | name <- srcfiles]
    examplesTextMap <- return E.examples

    el "h1" $ text "Interpreter for label-dependent Session Types"
    el "p" $ do
        text "You can find more information in the Paper by Peter Thiemann and Vasco T.Vasconcelos, available on "
        elAttr "a" ("href" =: "https://arxiv.org/abs/1911.00705" <> "target" =: "_blank") $ text "arxiv"
        text ".\n"
    el "p" $ text "The interpreter looks for a 'main' to evaluate, wich must be a 'val' without any free variables."

    -- create a dropdown to set the textarea text
    d <- dropdown (head srcfiles) (constDyn exampleFilesMap) def
    el "div" blank
    (b, _) <- elAttr' "button" ("id" =: "bInterpret") $ text "Interpret" 
    el "div" blank
    -- Interpret button click event
    let e = domEvent Click b
    el "div" blank
    -- Dynamic Text looked up in example ldgv files on dropdown change
    let dVal = _dropdown_value d
    let lookupExample = (\v -> maybe ("Did not find example file") id (Map.lookup v examplesTextMap))

    -- Textarea for source
    (tSrc, tSrcText) <- elAttr' "textarea" ("id" =: "tSrc") $ dynText $ fmap (T.pack.lookupExample) dVal
    el "div" blank

    -- set the new text in the textarea on dropdown selection
    performEvent_ $ ffor (fmap lookupExample $ _dropdown_change d) (\s -> liftJSM $ setSrc s)
    -- get the text inside the area on button click
    srcText <- performEvent $ ffor e $ (\v -> liftJSM $ textareaget)
   
    elAttr "textarea" ("id" =: "tOutput" <> "readonly" =: "readonly") blank
    el "div" blank
    -- interpret the source text
    doneEv <- performEvent ((\v -> liftIO $ do
                                              let s = T.unpack v
                                              -- clear the old output
                                              resetOutput
                                              -- interpret
                                              res <- try $ typecheck s >> I.interpret s :: IO (Either SomeException P.Value)
                                              -- print errors to the output if there are any
                                              --case res of
                                              --    Left e    -> (C.putStrLn $ show e) >> return ""
                                              --    Right val -> (setRes $ show val) >> (return $ show val)
                                              return $ either (\v -> "Error: " ++ show v) (\v -> "Result: " ++ show v) res
                            ) <$> srcText)

    -- Dynamic Text and Textarea for output
    output <- holdDyn "" $ fmap T.pack doneEv
    elAttr "p" ("id" =: "tResult") $ dynText output
    return ()
