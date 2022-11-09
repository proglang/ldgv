{
module ValueParsing.ValueGrammar (parseValues) where

import Control.Monad
import qualified Data.List as List

import Kinds
-- import Syntax
import ProcessEnvironment
import ValueParsing.ValueTokens (T(..))
import qualified ValueParsing.ValueTokens as T
}

%monad { T.Alex }
%lexer { (\f -> T.alexMonadScan >>= f) } { T _ T.EOF }
%error { parseError }
%tokentype { T }

--%name parseDecls Cmds
--%name parseType  Typ

%name parseValues Values

%token
    vunit     { T _ T.VUnitN }
    vlabel     { T _ T.VLabelN }
    vint      { T _ T.VIntN }
    vdouble      { T _ T.VDoubleN }
    vstring       { T _ T.VStringN }
    vpair         { T _ T.VPairN }
    vgreater      { T _ T.GreaterN }
    vlesser       { T _ T.LesserN }
    vcomma        { T _ T.CommaN }
    vparopen      { T _ T.ParOpenN }
    vparclose     { T _ T.ParCloseN }
    vsend         { T _ T.VSendN }

    int     { T _ (T.Int $$) }
    double  { T _ (T.Double $$) }
    string  { T _ (T.String $$) }
    label     { T _ (T.Label $$) }

--%right LET
--%nonassoc int double '(' var lab case natrec '()' lam rec fst snd new fork
--%right in
--%nonassoc '>' '<'
--%left '+' '-' NEG POS
--%left '*' '/'
--%left send recv
--%nonassoc APP


%%

-- Values : {[]}
--       | vunit { VUnit }
Values : vunit { VUnit }
       | vlabel label {VLabel $2}
       | vint int {VInt $2}
       | vdouble double {VDouble $2}
       | vstring string {VString (trimQuote(trimQuote $2)) }
       | vpair vlesser Values vcomma Values vgreater {VPair $3 $5}
       | vsend vparopen Values vparclose {VSend $3}

{


parseError (T (T.AlexPn _ line column) t) = do
  nextTokens <- filter (/= T.EOF) . (t:) <$> replicateM 9 (tokVal <$> T.alexMonadScan)
  let err | null nextTokens = "parse error: unexpected end of file"
          | otherwise       = mconcat
              [ "parse error at line "
              , show line
              , ", column "
              , show column
              , ": unexpected token"
              , if null (tail nextTokens) then " " else "s "
              , List.intercalate ", " $ showToken <$> nextTokens
              ]
  T.alexError err

showToken t = "›" ++ show t ++ "‹"

trimQuote :: String -> String
trimQuote (_:xs) = init xs
}
