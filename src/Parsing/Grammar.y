{
module Parsing.Grammar (parseDecls, parseType) where

import Control.Monad
import qualified Data.List as List

import Kinds
import Syntax
import Parsing.Tokens (T(..))
import qualified Parsing.Tokens as T
}

%monad { T.Alex }
%lexer { (\f -> T.alexMonadScan >>= f) } { T _ T.EOF }
%error { parseError }
%tokentype { T }

%name parseDecls Cmds
%name parseType  Typ

%token
    let     { T _ T.Let }
    rec     { T _ T.Rec }
    in      { T _ T.In }
    int     { T _ (T.Int $$) }
    double  { T _ (T.Double $$) }
    string  { T _ (T.Str $$) }
    var     { T _ (T.Var $$) }
    case    { T _ T.Case }
    fst     { T _ T.Fst }
    snd     { T _ T.Snd }
    of      { T _ T.Of }
    val     { T _ T.Val }
    type    { T _ T.Type }
    fork    { T _ T.Fork }
    new     { T _ T.New }
    send    { T _ T.Send }
    recv    { T _ T.Recv }
    create  { T _ T.Create }
    connect { T _ T.Connect }
    accept  { T _ T.Accept }

    -- for Binary Session Types; obsolete for Label Dependent ones
    select  { T _ T.Select }
    rcase   { T _ T.Rcase }
    close   { T _ T.Close }
    wait    { T _ T.Wait }

    expect  { T _ T.Expect }
    lab     { T _ (T.Lab $$) }
    kind    { T _ (T.Kind $$) }
    tid     { T _ (T.TID $$) }
    Unit    { T _ T.TUnit }
    Bot     { T _ T.TBot }
    Int     { T _ T.TInt }
    Nat     { T _ T.TNat }
    String  { T _ T.TString }
    Double  { T _ T.TDouble }
    natrec  { T _ T.NatRec }
    new_natrec { T _ T.NewNatRec }
    '()'    { T _ T.Unit }
    '->'    { T _ T.Arrow }
    '=>'    { T _ T.DoubleArrow }
    '{{'    { T _ T.OpenEqn }
    '}}'    { T _ T.CloseEqn }
    assume  { T _ T.Assume }
    '<:'    { T _ T.Subtype }
    '=:'    { T _ T.Equiv }
    ':'     { T _ T.Colon }
    ','     { T _ T.Comma }
    '.'     { T _ T.Dot }
    lam     { T _ T.Lambda }
    dualof  { T _ T.DualOf }
    '{'     { T _ (T.Sym '{') }
    '}'     { T _ (T.Sym '}') }
    '='     { T _ (T.Sym '=') }
    '+'     { T _ (T.Sym '+') }
    '-'     { T _ (T.Sym '-') }
    '*'     { T _ (T.Sym '*') }
    '/'     { T _ (T.Sym '/') }
    '('     { T _ (T.Sym '(') }
    ')'     { T _ (T.Sym ')') }
    '<'     { T _ (T.Sym '<') }
    '>'     { T _ (T.Sym '>') }
    '['     { T _ (T.Sym '[') }
    ']'     { T _ (T.Sym ']') }
    '!'     { T _ (T.Sym '!') }
    '?'     { T _ (T.Sym '?') }
    '"'     { T _ (T.Sym '"') }

%right LET
%nonassoc int double '(' var lab case natrec '()' lam rec fst snd new fork
%right in
%nonassoc '>' '<'
%left '+' '-' NEG POS
%left '*' '/'
%left send recv connect create accept
%nonassoc APP


%%

Cmds : {[]}
     | Cmd Cmds { $1 : $2 }

Cmd  : type tid ':' Mul kind '=' Typ    { DType $2 $4 $5 $7 }
     | val var ':' Mul Typ           { DSig $2 (inject $4) $5 }
     | val var Bindings Check '=' Exp      { DFun $2 $3 $6 $4 }
     | Typ '<:' Typ          { DSub $1 $3 }
     | Typ '=:' Typ          { DEqv $1 $3 }
     | '=' var Exp Exp       { DSubst $2 $3 $4 }
     | assume TENV Cmd      { DAssume $2 $3 }

Check : {Nothing}
      | ':' Typ {Just $2}

TENV : { [] }
     | '[' Assumptions ']' { $2 }

Binding : Mul '(' var ':' Typ ')' { ($1, $3, $5) }
Bindings : { [] }
         | Binding Bindings { $1 : $2 }

Assumption : var ':' Mul Typ { ($1, (inject $3, $4)) }

Assumptions :  { [] }
     | Assumption ',' Assumptions { $1 : $3 }

Mul : '!' { MOne }
    | { MMany }

Of : of {()} | {()}

AExp
    : int                    { Lit $ if $1 < 0 then LInt $1 else LNat $1 }
    | double                 { Lit $ LDouble $1 }
    | string                 { Lit $ LString (trimQuote $1) }
    | var                    { Var $1 }
    | lab                    { Lit $ LLab $1 }
    | '(' Exp ')'            { $2 }

Exp : let var '=' Exp in Exp %prec LET { Let $2 $4 $6 }
    | Exp '+' Exp           { Math $ Add $1 $3 }
    | Exp '-' Exp           { Math $ Sub $1 $3 }
    | Exp '*' Exp           { Math $ Mul $1 $3 }
    | Exp '/' Exp           { Math $ Div $1 $3 }
    | '(' Exp ')'           { $2 }
    | '+' Exp %prec POS     { $2 }
    | '-' Exp %prec NEG     { Math $ Neg $2 }
    | int                   { Lit $ if $1 < 0 then LInt $1 else LNat $1 }
    | double                { Lit $ LDouble $1 }
    | string                { Lit $ LString (trimQuote $1) }
    | Exp ':' Typ '=>' Typ  { Cast $1 $3 $5 }
    | var                   { Var $1 }
    | lab                   { Lit $ LLab $1 }
    | case Exp Of '{' ExpCases '}'  { Case $2 $5 }
    | natrec Exp '{' Exp ',' var '.' tid '.' '(' var ':' Typ ')' '.' Exp '}'
                            { NatRec $2 $4 $6 $8 $11 $13 $16 }
    | new_natrec var ':' var '.' tid '.' Typ '{' Exp ',' var '.' Exp '}'
                            { NewNatRec $2 $4 $6 $8 $10 $12 $14 }
    | '()'                  { Lit LUnit }
    | lam Mul '(' var ':' Typ ')' Exp                   { Lam $2 $4 $6 $8 }
    | rec var '(' var '.' Exp ')' Exp                   { Rec $2 $4 $6 $8 }
    | '<' Mul var '=' Exp ',' Exp '>'                   { Pair $2 $3 $5 $7 }
    | let '<' var ',' var '>' '=' Exp in Exp %prec LET  { LetPair $3 $5 $8 $10 }
    | fst Exp               { Fst $2 }
    | snd Exp               { Snd $2 }
    | new Typ               { New $2 }
    | fork Exp              { Fork $2 }
    | send Exp %prec send   { Send $2 }
    | recv Exp %prec recv   { Recv $2 }
    | create Exp %prec create       { Create $2 }
    | connect Exp Exp Typ %prec connect { Connect $2 $3 $4}
    | accept Exp Typ %prec accept           { Accept $2 $3 }
    | Exp Exp  %prec APP  { App $1 $2 }

Labs : lab          { [$1] }
     | lab ',' Labs { $1 : $3 }

TypCase : lab ':' Typ { ($1, $3) }

TypCases : TypCase            { [$1] }
      | TypCase ',' TypCases  { $1 : $3 }

ExpCase : lab ':' Exp { ($1, $3) }

ExpCases : ExpCase              { [$1] }
         | ExpCase ',' ExpCases { $1 : $3 }

ATyp : Unit                           { TUnit }
    | Int                             { TInt }
    | Nat                             { TNat }
    | Double                          { TDouble }
    | String                          { TString }
    | Bot                             { TBot }
    | '*'                             { TDyn }
    | tid                             { TName False $1 }
    | '{' Labs '}'                    { TLab $2 }
    | '[' Mul var ':' Typ ',' Typ ']' { TPair $3 $5 $7 }
    | '[' Mul Typ ',' Typ ']'         { TPair "#*" $3 $5 }
    | '{{' Exp '=' Exp ':' Typ '}}'   { TEqn $2 $4 $6 }
    | '(' Typ ')'                     { $2 }

Typ : ATyp                                    { $1 }
    | ATyp Mul '->' Typ                       { TFun $2 "#!" $1 $4 }
    | '(' var ':' Typ ')' Mul '->' Typ        { TFun $6 $2 $4 $8 }
    | '!' '(' var ':' Typ ')' Typ             { TSend $3 $5 $7 }
    | '?' '(' var ':' Typ ')' Typ             { TRecv $3 $5 $7 }
    | '!' ATyp '.' Typ                        { TSend "#!" $2 $4 }
    | '?' ATyp '.' Typ                        { TRecv "#?" $2 $4 }
    | case Exp Of '{' TypCases '}'            { TCase $2 $5 }
    | natrec Exp '{' Typ ',' tid '.' Typ '}'  { TNatRec $2 $4 $6 $8 }
    | dualof ATyp                             { dualof $2 }

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
