{
module Parsing.Grammar (parseDecls, parseType) where

import Kinds
import Parsing.Tokens (T(..))
import Syntax
import qualified Data.List as List
import qualified Parsing.Tokens as T
}

%name parseDecls Cmds
%name parseType  Typ

%tokentype { T }
%error { parseError }

%token
    let { T _ T.Let }
    rec { T _ T.Rec }
    in  { T _ T.In }
    int { T _ (T.Int $$) }
    var { T _ (T.Var $$) }
    case { T _ T.Case }
    fst { T _ T.Fst }
    snd { T _ T.Snd }
    of { T _ T.Of }
    val { T _ T.Val }
    type { T _ T.Type }
    fork { T _ T.Fork }
    new { T _ T.New }
    send { T _ T.Send }
    recv { T _ T.Recv }
    select { T _ T.Select }
    rcase { T _ T.Rcase }
    close { T _ T.Close }
    wait { T _ T.Wait }
    expect { T _ T.Expect }
    lab { T _ (T.Lab $$) }
    kind { T _ (T.Kind $$) }
    tid { T _ (T.TID $$) }
    Unit { T _ T.TUnit }
    Bot { T _ T.TBot }
    Int { T _ T.TInt }
    Nat { T _ T.TNat }
    natrec { T _ T.NatRec }
    '()' { T _ T.Unit }
    '->' { T _ T.Arrow }
    '{{' { T _ T.OpenEqn }
    '}}' { T _ T.CloseEqn }
    assume { T _ T.Assume }
    glb { T _ T.Glb }
    lub { T _ T.Lub }
    '<:' { T _ T.Subtype }
    '=:' { T _ T.Equiv }
    ':' { T _ T.Colon }
    ',' { T _ T.Comma }
    '.' { T _ T.Dot }
    lam { T _ T.Lambda }
    dualof { T _ T.DualOf }
    '{' { T _ (T.Sym '{') }
    '}' { T _ (T.Sym '}') }
    '=' { T _ (T.Sym '=') }
    '+' { T _ (T.Sym '+') }
    '-' { T _ (T.Sym '-') }
    '*' { T _ (T.Sym '*') }
    '/' { T _ (T.Sym '/') }
    '(' { T _ (T.Sym '(') }
    ')' { T _ (T.Sym ')') }
    '<' { T _ (T.Sym '<') }
    '>' { T _ (T.Sym '>') }
    '[' { T _ (T.Sym '[') }
    ']' { T _ (T.Sym ']') }
    '!' { T _ (T.Sym '!') }
    '?' { T _ (T.Sym '?') }

%right LET
%nonassoc int '(' var lab case natrec '()' lam rec fst snd new fork
%right in
%nonassoc '>' '<'
%left '+' '-' NEG
%left '*' '/'
%left send recv
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
     | Typ lub Typ          { DLub [] $1 $3 }
     | Typ glb Typ          { DGlb [] $1 $3 }

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

Vars : { [] }
     | var Vars { $1 : $2 }

Mul : '!' { MOne }
    | { MMany }

AExp
    : int                    { Lit $ if $1 < 0 then LInt $1 else LNat $1 }
    | var                    { Var $1 }
    | lab                    { Lit $ LLab $1 }
    | '(' Exp ')'            { $2 }

Exp : let var '=' Exp in Exp %prec LET { Let $2 $4 $6 }
    | Exp '+' Exp            { Math $ Add $1 $3 }
    | Exp '-' Exp            { Math $ Sub $1 $3 }
    | Exp '*' Exp            { Math $ Mul $1 $3 }
    | Exp '/' Exp            { Math $ Div $1 $3 }
    | '(' Exp ')'            { $2 }
    | '-' Exp %prec NEG      { Math $ Neg $2 }
    | int                    { Lit $ if $1 < 0 then LInt $1 else LNat $1 }
    | var                    { Var $1 }
    | lab                    { Lit $ LLab $1 }
    | case Exp of '{' ExpCases '}'  { Case $2 $5 }
    | natrec Exp '{' Exp ',' var '.' tid '.' '(' var ':' Typ ')' '.' Exp '}'
                             { NatRec $2 $4 $6 $8 $11 $13 $16 }
    | '()'                   { Lit LUnit }
    | lam Mul '(' var ':' Typ ')' Exp        { Lam $2 $4 $6 $8 }
    | rec var '(' var ':' Typ ')' ':' Typ '=' Exp        { Rec $2 $4 $6 $9 $11 }
    | '<' Mul var '=' Exp ',' Exp '>' { Pair $2 $3 $5 $7 }
    | let '<' var ',' var '>' '=' Exp in Exp %prec LET { LetPair $3 $5 $8 $10 }
    | fst Exp                { Fst $2 }
    | snd Exp                { Snd $2 }
    | new Typ { New $2 }
    | fork Exp { Fork $2 }
    | send Exp %prec send { Send $2 }
    | recv Exp %prec recv { Recv $2 }
    | Exp Exp  %prec APP  { App $1 $2 }


Labs : lab { [$1] }
     | lab ',' Labs { $1 : $3 }

TypCase : lab ':' Typ { ($1, $3) }

TypCases : TypCase { [$1] }
      | TypCase ',' TypCases { $1 : $3 }

ExpCase : lab ':' Exp { ($1, $3) }

ExpCases : ExpCase { [$1] }
         | ExpCase ',' ExpCases { $1 : $3 }

ATyp : Unit                          { TUnit }
    | Int                           { TInt }
    | Nat                           { TNat }
    | Bot                           { TBot }
    | tid                           { TName False $1 }
    | '{' Labs '}'                  { TLab $2 }
    | '[' Mul var ':' Typ ',' Typ ']'   { TPair $2 $3 $5 $7 }
    | '[' Mul Typ ',' Typ ']'   { TPair $2 "#*" $3 $5 }
    | '{{' Exp '=' Exp ':' Typ '}}' { TEqn $2 $4 $6 }
    | '(' Typ ')'                    { $2 }


Typ : ATyp                          { $1 }
    | '(' var ':' Typ ')' Mul '->' Typ  { TFun $6 $2 $4 $8 }
    | '!' '(' var ':' Typ ')' Typ   { TSend $3 $5 $7 }
    | '?' '(' var ':' Typ ')' Typ   { TRecv $3 $5 $7 }
    | '!' ATyp '.' Typ   { TSend "#!" $2 $4 }
    | '?' ATyp '.' Typ   { TRecv "#?" $2 $4 }
    | case Exp of '{' TypCases '}'  { TCase $2 $5 }
    | natrec Exp '{' Typ ',' tid '.' Typ '}' { TNatRec $2 $4 $6 $8 }
    | dualof ATyp                    { dualof $2 }

{

parseError :: [T] -> a
parseError ts = errorWithoutStackTrace $ "parse error at " ++ err
  where
    err = case ts of
      [] -> "end of file"
      T (T.AlexPn _ line column) _ : _ -> mconcat
        [ "line "
        , show line
        , ", column "
        , show column
        , ": "
        , List.intercalate ", " (show . T.tokVal <$> take 10 ts)
        ]

}
