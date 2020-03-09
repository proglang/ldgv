{-# LANGUAGE OverloadedStrings #-}
module SyntaxDescription (syntaxdescription) where

import Reflex.Dom.Core

syntaxdescription :: DomBuilder t m => m ()
syntaxdescription = elAttr "section" ("class" =: "syntax") $ do
    el "h2" $ text "Syntax"
    el "h3" $ text "Mutually recursive type definitions"
    el "p" $ do
        elcode "TID" 
        text "type identifier, starts with uppercase letter"
    el "h3" $ text "Declarations"
    el "p" $ do
        text "The top-level accepts a sequence of declarations"
        elcode "D"
        text "as follows:"
    el "ul" $ do
        el "li" $ text "type declaration with multiplicity and kind (accumulates assumptions)"
        el "li" $ text "variable declaration with multiplicity (accumulates assumptions)"
        el "li" $ text "function definition (runs type checker with assumptions)"
        el "li" $ text "run subtyping (runs subtyping with assumptions)"
        el "li" $ text "run type equivalence"
        pure ()
    divClass "rule" $ do
        divClass "base" $ do
            elcode "D"
            text "::= \"type\""
            elcode "TID" 
            text ":"
            elcode "m"
            elcode "K"
            text "="
            elcode "T"
        divClass "guard" $ do
            text "| \"val\""
            elcode "id"
            text ":"
            elcode "m"
            elcode "T"
            el "br" blank
        divClass "guard" $ do
            text "| \"val\""
            elcode "id"
            text "{"
            elcode "m" 
            text "\"(\" "
            elcode "id"
            text ":"
            elcode "T"
            text "\")\" "
            text "}"
            text "="
            elcode "M"
        divClass "guard" $ do
            text "|"
            elcode "T"
            text "<:"
            elcode "T"
        divClass "guard" $ do
            text "|"
            elcode "T"
            text "=:"
            elcode "T"
        divClass "guard" $ do
            text "|"
            elcode "T"
            text "\\/"
            elcode "T"
        divClass "guard" $ do
            text "|"
            elcode "T"
            text "/\\"
            elcode "T"
    
    el "h3" $ text "Multiplicities"
    divClass "rule" $ divClass "base" $ do
        elcode "m" 
        text "::="
        text "  | \"!\""
    el "p" $ text "unrestricted (nothing) or single-use"
    
    el "h3" $ text "Kinds"
    divClass "rule" $ divClass "base" $ do
        elcode "K"
        text "::= "
        text "\"~\""
        elcode "K'"
    divClass "rule" $ divClass "base" $ do
        elcode "K'"
        text "::= \"un\" | \"lin\" | \"unit\" | \"ssn\" | \"idx\""
    
    el "h3" $ text "Labels"
    divClass "rule" $ divClass "base" $ do
        elcode "lab"
        text "::= "
        text "\"'\""
        elcode "id"

    el "h3" $ text "Types"
    divClass "rule" $ do
        divClass "base" $ do
            elcode "T"
            text "::= \"Unit\""
        divClass "guard" $ do
            text "|"
            text "\"Int\""
        divClass "guard" $ do
            text "|"
            text "\"Bot\""
        divClass "guard" $ do
            text "|"
            elcode "TID"
        divClass "guard" $ do
            text "|"
            text "\"{\""
            elcode "lab"
            text "{ \",\""
            elcode "lab"
            text "} \"}\""
        divClass "guard" $ do
            text "|"
            text "\"case\""
            elcode "M"
            text "\"of\" \"{\""
            elcode "lab"
            text "\":\""
            elcode "T"
            text "{ \",\""
            elcode "lab"
            text "\":\""
            elcode "T"
            text "} \"}\""
        divClass "guard" $ do
            text "|"
            text "\"(\""
            elcode "id"
            text "\":\""
            elcode "T"
            text "\")\""
            elcode "m"
            text "\"->\""
            elcode "T"
        divClass "guard" $ do
            text "|"
            text "\"[\""
            elcode "m"
            elcode "id"
            text "\":\""
            elcode "T"
            text "\",\""
            elcode "T"
            text "\"]\""
        divClass "guard" $ do
            text "|"
            text "\"!\""
            text "\"(\""
            elcode "id"
            text "\":\""
            elcode "T"
            text "\")\""
            elcode "T"
        divClass "guard" $ do
            text "|"
            text "\"?\""
            text "\"(\""
            elcode "id"
            text "\":\""
            elcode "T"
            text "\")\""
            elcode "T"
        divClass "guard" $ do
            text "|"
            text "\"[\""
            elcode "m"
            elcode "T"
            text "\",\""
            elcode "T"
            text "\"]\""
        divClass "guard" $ do
            text "|"
            text "\"!\""
            elcode "T"
            text "\".\""
            elcode "T"
        divClass "guard" $ do
            text "|"
            text "\"?\""
            elcode "T"
            text "\".\""
            elcode "T"
        divClass "guard" $ do
            text "|"
            text "\"{{\""
            elcode "M"
            text "\"=\""
            elcode "M"
            text "\":\""
            elcode "T"
            text "\"}}\""
        divClass "guard" $ do
            text "|"
            text "\"dualof\""
            elcode "T"

    divClass "rule" $ do
        divClass "base" $ do
            elcode "M"
            text "::= \"()\""
        divClass "guard" $ do
            text "|"
            elcode "lab"
        divClass "guard" $ do
            text "|"
            elcode "id"
        divClass "guard" $ do
            text "|"
            text "\"func\""
            elcode "m"
            text "\"(\""
            elcode "id"
            text "\")\""
            elcode "T"
            text "\")\""
            elcode "M"
        divClass "guard" $ do
            text "|"
            text "\"rec\""
            elcode "id"
            text "\"(\""
            elcode "id"
            text "\")\""
            elcode "T"
            text "\")\""
            text "\":\""
            elcode "T"
            text "\"=\""
            elcode "M"
        divClass "guard" $ do
            text "|"
            elcode "M"
            elcode "M"
        divClass "guard" $ do
            text "|"
            text "\"let\""
            elcode "id"
            text "\"=\""
            elcode "M"
            text "\"in\""
            elcode "M"
        divClass "guard" $ do
            text "|"
            text "\"<\""
            elcode "m"
            elcode "id"
            text "\"=\""
            elcode "M"
            text "\",\""
            elcode "M"
            text "\">\""
            elcode "m"
        divClass "guard" $ do
            text "|"
            text "\"let\""
            text "\"<\""
            elcode "id"
            text "\",\""
            elcode "id"
            text "\">\""
            text "\"=\""
            elcode "M"
            text "\"in\""
            elcode "M"
        divClass "guard" $ do
            text "|"
            text "\"fst\""
            elcode "M"
        divClass "guard" $ do
            text "|"
            text "\"snd\""
            elcode "M"
        divClass "guard" $ do
            text "|"
            text "\"new\""
            elcode "M"
        divClass "guard" $ do
            text "|"
            text "\"fork\""
            elcode "M"
        divClass "guard" $ do
            text "|"
            text "\"send\""
            elcode "M"
        divClass "guard" $ do
            text "|"
            text "\"recv\""
            elcode "M"
        divClass "guard" $ do
            text "|"
            text "\"(\""
            elcode "M"
            text "\")\""
        divClass "guard" $ do
            text "|"
            text "\"case\""
            elcode "M"
            text "\"of\" "
            text "\"{\""
            text "\"=\""
            elcode "lab"
            text "\":\""
            elcode "M"
            text "{"
            text "\",\""
            elcode "lab"
            text "\":\""
            elcode "M"
            text "}"
            text "\"}\""
 
elcode s = el "code" $ text s
