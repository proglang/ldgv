{
module Grammar where
import qualified Tokens as T
import Kinds
import Syntax
}

%name parseCalc
%tokentype { T.Token }
%error { parseError }

%token
    let { T.Let }
    rec { T.Rec }
    in  { T.In }
    int { T.Int $$ }
    double { T.Double $$ }
    string { T.Str $$ }
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
    Double { T.TDouble }
    String { T.TString }
    Nat { T.TNat }
    natrec { T.NatRec }
    new_natrec { T.NewNatRec }
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
%nonassoc int double string '(' var lab case natrec '()' lam rec fst snd new fork
%right in
%nonassoc '>' '<'
%left '+' '-' NEG
%left '*' '/'
%left send recv
%nonassoc APP


%%

Cmds : {[]}
     | Cmd Cmds { $1 : $2 }

Cmd  : type tid OptKind '=' Typ    { let (m,k) = $3 in DType $2 m k $5 }
     | val var ':' Mul Typ           { DSig $2 (inject $4) $5 }
     | val var Bindings Check '=' Exp      { DFun $2 $3 $6 $4 }
     | Typ '<:' Typ          { DSub $1 $3 }
     | Typ '=:' Typ          { DEqv $1 $3 }
     | '=' var Exp Exp       { DSubst $2 $3 $4 }
     | assume TENV Cmd      { DAssume $2 $3 }
     | Typ lub Typ          { DLub [] $1 $3 }
     | Typ glb Typ          { DGlb [] $1 $3 }

OptKind : ':' Mul kind      { ($2, $3) }
        |                   { (MMany, Kun) }

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

Of : of {()}
   | {()}

MType : ':' Typ          { Just $2 }
      |                  { Nothing }

AExp
    : int                    { if $1 < 0 then Int $1 else Nat $1 }
    | double                 { Double $1 }
    | string                 { Str $1 }
    | var                    { Var $1 }
    | lab                    { Lab $1 }
    | '(' Exp ')'            { $2 }
    | '(' Exp ':' Typ ')'    { Typed $2 $4 }

Exp : let var MType '=' Exp in Exp %prec LET { Let $2 $5 $7 } -- MType = $3
    | Exp '+' Exp            { Plus $1 $3 }
    | Exp '-' Exp            { Minus $1 $3 }
    | Exp '*' Exp            { Times $1 $3 }
    | Exp '/' Exp            { Div $1 $3 }
    | '-' Exp %prec NEG      { Negate $2 }
    | AExp                   { $1 }
    | case Exp Of '{' ExpCases '}'  { Case $2 $5 }
    | natrec Exp '{' Exp ',' var '.' tid '.' '(' var ':' Typ ')' '.' Exp '}'
                             { NatRec $2 $4 $6 $8 $11 $13 $16 }
    | new_natrec var ':' var '.' tid '.' Typ '{' Exp ',' var '.' Exp '}'
                             { NewNatRec $2 $4 $6 $8 $10 $12 $14 }
    | '()'                   { Unit }
    | lam Mul '(' var ':' Typ ')' Exp        { Lam $2 $4 $6 $8 }
    | rec var '(' var ':' Typ ')' ':' Typ '=' Exp        { Rec $2 $4 $6 $9 $11 }
    | '<' Mul var MType '=' Exp ',' Exp '>' { Pair $2 $3 $6 $8 } -- MType = $4
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

ATyp : Unit                         { TUnit }
    | Double                        { TDouble }
    | String                        { TString }
    | Int                           { TInt }
    | Nat                           { TNat }
    | Bot                           { TBot }
    | '*'                           { TDyn }
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
    | case Exp Of '{' TypCases '}'  { TCase $2 $5 }
    | natrec Exp '{' Typ ',' tid '.' Typ '}' { TNatRec $2 $4 $6 $8 }
    | dualof ATyp                    { dualof $2 }

{

parseError :: [T.Token] -> a
parseError ts = error ("Parse error at: " ++ show (take 10 ts))

}
