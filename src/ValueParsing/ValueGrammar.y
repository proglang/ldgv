{
module ValueParsing.ValueGrammar (parseValues, parseMessages) where

import Control.Monad
import qualified Data.List as List
import qualified Data.Set as Set

import Kinds
import Syntax
import ProcessEnvironment
import ValueParsing.ValueTokens (T(..))
import qualified ValueParsing.ValueTokens as T
import Networking.Messages
}

%monad { T.Alex }
%lexer { (\f -> T.alexMonadScan >>= f) } { T _ T.EOF }
%error { parseError }
%tokentype { T }

--%name parseDecls Cmds
--%name parseType  Typ

%name parseValues Values
%name parseMessages Messages
-- %name parseSStringTypeElement SStringTypeElement
-- %name parseSStringTypeElements SStringTypeElements
-- %name parseSStringTypeArray SStringTypeArray

%token
    vunit         { T _ T.VUnit }
    vlabel        { T _ T.VLabel }
    vint          { T _ T.VInt }
    vdouble       { T _ T.VDouble }
    vstring       { T _ T.VString }
    vchan         { T _ T.VChan}
    vsend         { T _ T.VSend }
    vpair         { T _ T.VPair }
    vtype         { T _ T.VType }
    vfunc         { T _ T.VFunc }
    vdyncast      { T _ T.VDynCast }
    vfunccast     { T _ T.VFuncCast }
    vrec          { T _ T.VRec }
    vnewnatrec    { T _ T.VNewNatRec }

    tunit         { T _ T.TUnit }
    tint          { T _ T.TInt }
    tdouble       { T _ T.TDouble }
    tbot          { T _ T.TBot }
    tdyn          { T _ T.TDyn }
    tnat          { T _ T.TNat }
    tstring       { T _ T.TString }
    tnatleq       { T _ T.TNatLeq }
    tnatrec       { T _ T.TNatRec }
    tvar          { T _ T.TVar }
    tabs          { T _ T.TAbs }
    tname         { T _ T.TName }
    tlab          { T _ T.TLab }
    tfun          { T _ T.TFun }
    tpair         { T _ T.TPair }
    tsend         { T _ T.TSend }
    trecv         { T _ T.TRecv }
    tcase         { T _ T.TCase }
    teqn          { T _ T.TEqn }
    tsingle       { T _ T.TSingle }

    elet          { T _ T.ELet }
    emath         { T _ T.EMath }
    elit          { T _ T.ELit }
    esucc         { T _ T.ESucc }
    enatrec       { T _ T.ENatRec }
    enewnatrec    { T _ T.ENewNatRec }
    evar          { T _ T.EVar }
    elam          { T _ T.ELam }
    erec          { T _ T.ERec }
    eapp          { T _ T.EApp }
    epair         { T _ T.EPair }
    eletpair      { T _ T.ELetPair }
    efst          { T _ T.EFst }
    esnd          { T _ T.ESnd }
    efork         { T _ T.EFork }
    enew          { T _ T.ENew }
    esend         { T _ T.ESend }
    erecv         { T _ T.ERecv }
    ecase         { T _ T.ECase }
    ecast         { T _ T.ECast }

    madd          { T _ T.MAdd }
    msub          { T _ T.MSub }
    mmul          { T _ T.MMul }
    mdiv          { T _ T.MDiv }
    mneg          { T _ T.MNeg }

    mone          { T _ T.MOne }
    mmany         { T _ T.MMany }

    lint          { T _ T.LInt }
    lnat          { T _ T.LNat }
    ldouble       { T _ T.LDouble }
    llab          { T _ T.LLab }
    lunit         { T _ T.LUnit }
    lstring       { T _ T.LString }

    sfunctype     { T _ T.SFuncType }
    slabeltype    { T _ T.SLabelType }
    sstringexparray { T _ T.SStringExpArray }
    sstringtypearray { T _ T.SStringTypeArray }
    sstringarray  { T _ T.SStringArray }
    svaluesarray  { T _ T.SValuesArray }

    nintroduce    { T _ T.NIntroduce }
    nintroduceclient    { T _ T.NIntroduceClient }
    nintroduceserver    { T _ T.NIntroduceServer }
    nnewvalue     { T _ T.NNewValue }
    nsyncincoming { T _ T.NSyncIncoming }
    nrequestsync  { T _ T.NRequestSync }
    nchangepartneraddress {T _ T.NChangePartnerAddress }
    
    gunit         { T _ T.GUnit }
    glabel        { T _ T.GLabel }
    gfunc         { T _ T.GFunc }
    gpair         { T _ T.GPair }
    gnat          { T _ T.GNat }
    gnatleq       { T _ T.GNatLeq }
    gint          { T _ T.GInt }
    gdouble       { T _ T.GDouble }
    gstring       { T _ T.GString }

    penv          { T _ T.PEnv }
    penventry     { T _ T.PEnvEntry }


    int           { T _ (T.Int $$) }
    integer       { T _ (T.Integer $$) }
    double        { T _ (T.Double $$) }
    string        { T _ (T.String $$) }
    bool          { T _ (T.Bool $$) }

    '{'     { T _ (T.Sym '{') }
    '}'     { T _ (T.Sym '}') }
--    '='     { T _ (T.Sym '=') }
--    '+'     { T _ (T.Sym '+') }
--    '-'     { T _ (T.Sym '-') }
--    '*'     { T _ (T.Sym '*') }
--    '/'     { T _ (T.Sym '/') }
    '('     { T _ (T.Sym '(') }
    ')'     { T _ (T.Sym ')') }
--    '<'     { T _ (T.Sym '<') }
--    '>'     { T _ (T.Sym '>') }
    '['     { T _ (T.Sym '[') }
    ']'     { T _ (T.Sym ']') }
--    '!'     { T _ (T.Sym '!') }
--    '?'     { T _ (T.Sym '?') }
--    '"'     { T _ (T.Sym '"') }
    ','     { T _ (T.Sym ',') }

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
       | vlabel '(' String ')' {VLabel $3 }
       | vint '(' int ')' {VInt $3}
       | vdouble '(' double ')' {VDouble $3}
       | vstring '(' String ')' {VString $3 }
       -- | vchan '(' SValuesArray ')' '(' int ')' '(' SValuesArray ')' '(' int ')' '(' String ')' '(' String ')' '(' String ')' '(' String ')' {VChanSerial $3 $6 $9 $12 $15 $18 $21 $24 }
       | vsend '(' Values ')' {VSend $3}
       | vpair '(' Values ')' '(' Values ')' {VPair $3 $6}
       | vtype '(' Type ')' {VType $3}
       | vfunc '(' PEnv ')' '(' String ')' '(' Exp ')' {VFunc $3 $6 $9}
       | vdyncast '(' Values ')' '(' GType ')' {VDynCast $3 $6}
       | vfunccast '(' Values ')' '(' SFuncType ')'  '(' SFuncType ')' {VFuncCast $3 $6 $9}
       | vrec '(' PEnv ')' '(' String ')' '(' String ')' '(' Exp ')' '(' Exp ')' {VRec $3 $6 $9 $12 $15}
       | vnewnatrec '(' PEnv ')' '(' String ')' '(' String ')' '(' String ')' '(' Type ')' '(' Exp ')' '(' String ')' '(' Exp ')' {VNewNatRec $3 $6 $9 $12 $15 $18 $21 $24}

String : string {trimQuote $1}


Mult : mone { MOne }
     | mmany { MMany }

Literal : lint '(' int ')' {LInt $3}
        | lnat '(' int ')' {LNat $3}
        | ldouble '(' double ')' {LDouble $3}
        | llab '(' String ')' {LLab $3}
        | lunit {LUnit}
        | lstring '(' String ')' {LLab $3}

SFuncType : sfunctype '(' PEnv ')' '(' String ')' '(' Type ')' '(' Type ')' {FuncType $3 $6 $9 $12}

Type : tunit {TUnit}
     | tint {TInt}
     | tdouble {TDouble}
     | tbot {TBot}
     | tdyn {TDyn}
     | tnat {TNat}
     | tstring {TString}
     | tnatleq '(' integer ')' {TNatLeq $3}
     | tnatrec '(' Exp ')' '(' Type ')' '(' String ')' '(' Type ')' {TNatRec $3 $6 $9 $12}
     | tvar '(' bool ')' '(' String ')' {TVar $3 $6}
     | tabs '(' String ')' '(' Type ')' '(' Type ')' {TAbs $3 $6 $9}
     | tname '(' bool ')' '(' String ')' {TName $3 $6}
     | tlab '(' SStringArray ')' {TLab $3}
     | tfun '(' Mult ')' '(' String ')' '(' Type ')' '(' Type ')' {TFun $3 $6 $9 $12}
     | tpair '(' String ')' '(' Type ')' '(' Type ')' {TPair $3 $6 $9}
     | tsend '(' String ')' '(' Type ')' '(' Type ')' {TSend $3 $6 $9}
     | trecv '(' String ')' '(' Type ')' '(' Type ')' {TRecv $3 $6 $9}
     | tcase '(' Exp ')' '(' SStringTypeArray ')' {TCase $3 $6}
     | teqn '(' Exp ')' '(' Exp ')' '(' Type ')' {TEqn $3 $6 $9}
     | tsingle '(' String ')' {TSingle $3}

Exp : elet '(' String ')' '(' Exp ')' '(' Exp ')' {Let $3 $6 $9}
    | emath '(' MathOp ')' {Math $3}
    | elit '(' Literal ')' {Lit $3}
    | esucc '(' Exp ')' {Succ $3}
    | enatrec '(' Exp ')' '(' Exp ')' '(' String ')' '(' String ')' '(' String ')' '(' Type ')' '(' Exp ')' {NatRec $3 $6 $9 $12 $15 $18 $21}
    | enewnatrec '(' String ')' '(' String ')' '(' String ')' '(' Type ')' '(' Exp ')' '(' String ')' '(' Exp ')' {NewNatRec $3 $6 $9 $12 $15 $18 $21}
    | evar '(' String ')' {Var $3}
    | elam '(' Mult ')' '(' String ')' '(' Type ')' '(' Exp ')' {Lam $3 $6 $9 $12}
    | erec '(' String ')' '(' String ')' '(' Exp ')' '(' Exp ')' {Rec $3 $6 $9 $12}
    | eapp '(' Exp ')' '(' Exp ')' {App $3 $6}
    | epair '(' Mult ')' '(' String ')' '(' Exp ')' '(' Exp ')' {Pair $3 $6 $9 $12}
    | eletpair '(' String ')' '(' String ')' '(' Exp ')' '(' Exp ')' {LetPair $3 $6 $9 $12}
    | efst '(' Exp ')' {Fst $3}
    | esnd '(' Exp ')' {Snd $3}
    | efork '(' Exp ')' {Fork $3}
    | enew '(' Type ')' {New $3}
    | esend '(' Exp ')' {Send $3}
    | erecv '(' Exp ')' {Recv $3}
    | ecase '(' Exp ')' '(' SStringExpArray ')' {Case $3 $6}
    | ecast '(' Exp ')' '(' Type ')' '(' Type ')'  {Cast $3 $6 $9}


MathOp : madd '(' Exp ')' '(' Exp ')' {Add $3 $6}
       | msub '(' Exp ')' '(' Exp ')' {Sub $3 $6}
       | mmul '(' Exp ')' '(' Exp ')' {Mul $3 $6}
       | mdiv '(' Exp ')' '(' Exp ')' {Div $3 $6}
       | mneg '(' Exp ')' {Neg $3 }

GType : gunit {GUnit}
      | glabel '(' LabelType ')' {GLabel (Set.fromList $3) }
      | gfunc '(' Mult ')' {GFunc $3}
      | gpair {GPair}
      | gnat {GNat}
      | gnatleq '(' integer ')' {GNatLeq $3}
      | gint {GInt}
      | gdouble {GDouble}
      | gstring {GString}

Messages : nintroduce '(' String ')' {Introduce $3}
         | nintroduceclient '(' String ')' '(' String ')' {IntroduceClient $3 $6}
         | nintroduceserver '(' String ')' {IntroduceServer $3}
         | nnewvalue '(' String ')''(' Values ')' {NewValue $3 $6}
         | nsyncincoming '(' String ')''(' SValuesArray ')' {SyncIncoming $3 $6}
         | nrequestsync '(' String ')' {RequestSync $3}
         | nchangepartneraddress '(' String ')' '(' String ')' '(' String ')' {ChangePartnerAddress $3 $6 $9}


PEnvEntry : penventry '(' String ')' '(' Values ')' {($3, $6)}

PEnv : penv '[' PEnvElements ']' { $3 }

PEnvElements : PEnvEntry ',' PEnvElements {$1 : $3}
             | PEnvEntry {[$1]}
             | {- empty -} {[]}

SStringArray : sstringarray '[' SStringElements ']' {$3}

SStringElements : String ',' SStringElements {$1 : $3}
                     | String {[$1]}
                     | {- empty -} {[]}

SStringTypeArray : sstringtypearray '[' SStringTypeElements ']' {$3}

SStringTypeElements : SStringTypeElement ',' SStringTypeElements {$1 : $3}
                    | SStringTypeElement {[$1]}
                    | {- empty -} {[]}

SStringTypeElement : '(' '(' String ')' '(' Type ')' ')'  {($3, $6)}

SStringExpArray : sstringexparray '[' SStringExpElements ']' {$3}

SStringExpElements : SStringExpElement ',' SStringExpElements {$1 : $3}
                    | SStringExpElement {[$1]}
                    | {- empty -} {[]}

SStringExpElement : '(' '(' String ')' '(' Exp ')' ')'  {($3, $6)}

SValuesArray : svaluesarray '[' SValuesElements ']' {$3}

SValuesElements : Values ',' SValuesElements {$1 : $3}
                | Values {[$1]}
                | {- empty -} {[]}

LabelType : slabeltype '{' SStringElements '}' {$3}

     

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
