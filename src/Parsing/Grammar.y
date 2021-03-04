{
module Parsing.Grammar (parseDecls, parseType) where
import qualified Parsing.Tokens as T
import Kinds
import Syntax
}

%name parseDecls Cmds
%name parseType  Typ

%tokentype { T.Token }
%error { parseError }

%token
    let { T.Let }
    rec { T.Rec }
    in  { T.In }
    int { T.Int $$ }
    var { T.Var $$ }
    case { T.Case }
    fst { T.Fst }
    snd { T.Snd }
    of { T.Of }
    val { T.Val }
    type { T.Type }
    fork { T.Fork }
    new { T.New }
    send { T.Send }
    recv { T.Recv }
    select { T.Select }
    rcase { T.Rcase }
    close { T.Close }
    wait { T.Wait }
    expect { T.Expect }
    lab { T.Lab $$ }
    kind { T.Kind $$ }
    tid { T.TID $$ }
    Unit { T.TUnit }
    Bot { T.TBot }
    Int { T.TInt }
    Nat { T.TNat }
    natrec { T.NatRec }
    '()' { T.Unit }
    '->' { T.Arrow }
    '{{' { T.OpenEqn }
    '}}' { T.CloseEqn }
    assume { T.Assume }
    glb { T.Glb }
    lub { T.Lub }
    '<:' { T.Subtype }
    '=:' { T.Equiv }
    ':' { T.Colon }
    ',' { T.Comma }
    '.' { T.Dot }
    lam { T.Lambda }
    dualof { T.DualOf }
    '{' { T.Sym '{' }
    '}' { T.Sym '}' }
    '=' { T.Sym '=' }
    '+' { T.Sym '+' }
    '-' { T.Sym '-' }
    '*' { T.Sym '*' }
    '/' { T.Sym '/' }
    '(' { T.Sym '(' }
    ')' { T.Sym ')' }
    '<' { T.Sym '<' }
    '>' { T.Sym '>' }
    '[' { T.Sym '[' }
    ']' { T.Sym ']' }
    '!' { T.Sym '!' }
    '?' { T.Sym '?' }

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

parseError :: [T.Token] -> a
parseError ts = errorWithoutStackTrace ("Parse error at: " ++ show (take 10 ts))

}
