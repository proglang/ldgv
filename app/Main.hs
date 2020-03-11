{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Map as Map
import Reflex.Dom.Core
import Language.Javascript.JSaddle
import Language.Javascript.JSaddle (eval, liftJSM)

import SyntaxDescription (syntaxdescription)
import Interpreter as I
import Typechecker (typecheck)
import qualified Examples as E
import Control.Exception (try, SomeException)
import ProcessEnvironment as P (Value) 

-- head of document
widgetHead :: DomBuilder t m => m ()
widgetHead = do
  el "title" $ text "LDGV Interpreter"
  elAttr "link" ("rel" =: "stylesheet" <> "href" =: "main.css" <> "type" =: "text/css") blank

-- header for description and syntax
header :: DomBuilder t m => m ()
header = do
    el "h1" $ text "Interpreter for label-dependent Session Types"
    el "p" $ do
        text "You can find more information in the paper by Peter Thiemann and Vasco T.Vasconcelos, available on "
        elAttr "a" ("href" =: "https://arxiv.org/abs/1911.00705" <> "target" =: "_blank") $ text "arxiv"
        text ".\n"

main = mainWidgetWithHead widgetHead $ divClass "wrapper" $ do
    el "header" header
    elAttr "section" ("class" =: "interpreter") $ do
        let srcfiles = E.filenames
        -- create a map for lookup of all texts (by dropdown value)
        let exampleFilesMap = Map.fromList [(name, T.pack name) | name <- srcfiles]
        examplesTextMap <- return E.examples

        -- create a dropdown to set the textarea text
        d <- dropdown (head srcfiles) (constDyn exampleFilesMap) def
        (b, _) <- elAttr' "button" ("id" =: "bInterpret") $ text "Interpret" 
        -- Interpret button click event
        let e = domEvent Click b
        -- Dynamic Text looked up in example ldgv files on dropdown change
        let dVal = _dropdown_value d
        let lookupExample = (\v -> maybe ("Did not find example file") id (Map.lookup v examplesTextMap))

        -- Textarea for source
        elAttr "textarea" ("id" =: "tSrc" <> "class" =: "source_textarea" <>"spellcheck" =: "false") $ dynText $ fmap (T.pack.lookupExample) dVal

        -- set the new text in the textarea on dropdown selection
        performEvent_ $ ffor (fmap lookupExample $ _dropdown_change d) (\s -> liftJSM $ setSrc s)
        -- get the text inside the area on button click
        srcText <- performEvent $ ffor e $ (const $ liftJSM $ textareaget)
   
        elAttr "textarea" ("id" =: "tOutput" <> "class" =: "output_textarea" <> "readonly" =: "readonly" <> "spellcheck" =: "false") blank
        el "div" blank
        -- interpret the source text
        doneEv <- performEvent ((\v -> liftIO $ do
                                                  let s = T.unpack v
                                                  -- clear the old output
                                                  resetOutput
                                                  -- interpret
                                                  res <- try $ typecheck s >> I.interpret s :: IO (Either SomeException P.Value)
                                                  -- print errors to the output if there are any
                                                  return $ either (\v -> "Error: " ++ show v) (\v -> "Result: " ++ show v) res
                                ) <$> srcText)

        -- Dynamic Text and Textarea for output
        output <- holdDyn "" $ fmap T.pack doneEv
        elAttr "p" ("id" =: "tResult") $ dynText output

        -- describe our syntax
        syntaxdescription 

        _ <- elDynHtmlAttr' "a" ("href" =: "https://github.com/hagnernils/ldgv" <> "target" =: "_blank" <> "class" =: "github-corner" <> "aria-label" =: "View source on GitHub") $ constDyn $ T.pack $ ghSourceSvg ++ ghSourceStyle
        return ()

-- svg and style for the source on github-button
ghSourceSvg :: String
ghSourceSvg = "<svg width=\"80\" height=\"80\" viewBox=\"0 0 250 250\" style=\"fill:#151513; color:#fff; position: absolute; top: 0; border: 0; right: 0;\" aria-hidden=\"true\"><path d=\"M0,0 L115,115 L130,115 L142,142 L250,250 L250,0 Z\"></path><path d=\"M128.3,109.0 C113.8,99.7 119.0,89.6 119.0,89.6 C122.0,82.7 120.5,78.6 120.5,78.6 C119.2,72.0 123.4,76.3 123.4,76.3 C127.3,80.9 125.5,87.3 125.5,87.3 C122.9,97.6 130.6,101.9 134.4,103.2\" fill=\"currentColor\" style=\"transform-origin: 130px 106px;\" class=\"octo-arm\"></path><path d=\"M115.0,115.0 C114.9,115.1 118.7,116.5 119.8,115.4 L133.7,101.6 C136.9,99.2 139.9,98.4 142.2,98.6 C133.8,88.0 127.5,74.4 143.8,58.0 C148.5,53.4 154.0,51.2 159.7,51.0 C160.3,49.4 163.2,43.6 171.4,40.1 C171.4,40.1 176.1,42.5 178.8,56.2 C183.1,58.6 187.2,61.8 190.9,65.4 C194.5,69.0 197.7,73.2 200.1,77.6 C213.8,80.2 216.3,84.9 216.3,84.9 C212.7,93.1 206.9,96.0 205.4,96.6 C205.1,102.4 203.0,107.8 198.3,112.5 C181.9,128.9 168.3,122.5 157.7,114.1 C157.9,116.9 156.7,120.9 152.7,124.9 L141.0,136.5 C139.8,137.7 141.6,141.9 141.8,141.8 Z\" fill=\"currentColor\" class=\"octo-body\"></path></svg>"
ghSourceStyle :: String
ghSourceStyle = "<style>.github-corner:hover .octo-arm{animation:octocat-wave 560ms ease-in-out}@keyframes octocat-wave{0%,100%{transform:rotate(0)}20%,60%{transform:rotate(-25deg)}40%,80%{transform:rotate(10deg)}}@media (max-width:500px){.github-corner:hover .octo-arm{animation:none}.github-corner .octo-arm{animation:octocat-wave 560ms ease-in-out}}</style>"

-- read the text inside our textarea
textareaget :: JSM T.Text
textareaget = valToText $ eval ("document.getElementById('tSrc').value" :: T.Text)

-- set the text inside an element given by id
setHtmlElement :: String -> String -> JSM ()
setHtmlElement ident s = do
    _ <- eval (T.pack $ "document.getElementById('" ++ ident ++ "').value = " ++ (show s))
    pure ()

setSrc :: String -> JSM ()
setSrc = setHtmlElement "tSrc"

setRes :: String -> JSM ()
setRes = setHtmlElement "tResult"

resetOutput :: JSM ()
resetOutput = setHtmlElement "tOutput" ""

