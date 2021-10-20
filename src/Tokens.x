{
module Tokens where
import Kinds
}

%wrapper "basic"

$digit = 0-9                    -- digits
$alpha = [a-zA-Z]               -- alphabetic characters
$lower = [a-z]
$upper = [A-Z]
$special    = [\.\;\,\$\|\*\+\?\#\~\-\{\}\(\)\[\]\^\/]

@label      = [\'\`] [$alpha $digit]+
@ident      = $lower [$alpha $digit \_ \']*
@tident     = $upper [$alpha $digit \_ \']*
@string     = [\"] ($printable # [\\\"] | [\\][\"\\])* [\"]
@kind       = "~" ("un" | "lin" | "unit" | "ssn" | "idx")

tokens :-

  $white+                               ;
  "--".*                                ;
  "{-" (.| \n)* "-}"                    ;
  assume                                { const Assume }
  case                                  { const Case }
  type                                  { const Type }
  let                                   { const Let }
  rec                                   { const Rec }
  fst                                   { const Fst }
  snd                                   { const Snd }
  in                                    { const In }
  of                                    { const Of }
  val                                   { const Val }
  fork                                  { const Fork }
  new                                   { const New }
  send                                  { const Send }
  recv                                  { const Recv }
  select                                { const Select }
  rcase                                 { const Rcase }
  close                                 { const Close }
  wait                                  { const Wait }
  expect                                { const Expect }
  ("+"|"-")? $digit+ "." $digit+        { Double . read . removePrecedingPlus }
  ("+"|"-")? $digit+                    { Int . read . removePrecedingPlus }
  Bot                                   { const TBot }
  Unit                                  { const TUnit }
  Double                                { const TDouble }
  String                                { const TString }
  Int                                   { const TInt }
  natrec                                { const NatRec } 
  Nat                                   { const TNat }
  dualof                                { const DualOf }
  "_|_"                                 { const TBot }
  "|-|"                                 { const Glb }
  "\/"                                  { const Lub }
  "()"                                  { const Unit }
  "->"                                  { const Arrow }
  "{{"                                  { const OpenEqn }
  "}}"                                  { const CloseEqn }
  "<:"                                  { const Subtype }
  "=:"                                  { const Equiv }
  ":"                                   { const Colon }
  ","                                   { const Comma }
  "."                                   { const Dot }
  fn |   𝜆                              { const Lambda }
  [\=\+\-\*\/\(\)\:\!\?\{\}\[\]\<\>]    { Sym . head }
  @label                                { Lab . tail }
  @kind                                 { Kind . read . ('K':) . tail }
  @ident                                { Var }
  @tident                               { TID }
  @string                               { Str . cleanup . tail . init }         -- needs cleanup

{

-- cleanup a string token
-- character following a backslash is taken literally
cleanup :: String -> String
cleanup "" = ""
cleanup ('\\':x:xs) = x:cleanup xs
cleanup (x:xs) = x:cleanup xs

removePrecedingPlus :: String -> String
removePrecedingPlus ('+':chars) = chars
removePrecedingPlus s = s

-- Each action has type :: String -> Token

-- The token type:
data Token =
        Let             |
        Rec             |
        Fst             |
        Snd             |
        Case            |       
        Assume          |       
        In              |
        Of              |
        Val             |
        Fork            |
        New             |
        Send            |
        Recv            |
        Select          |
        Rcase           |
        Close           |
        Wait            |
        Expect          |
        Type            |
        Sym Char        |
        Kind Kind       |
        Lab String      |
        Var String      |
        TID String      |
        Unit            |
        TBot            |
        TUnit           |
        TInt            |
        TNat            |
        TDouble         |
        TString         |
        NatRec          |
        Subtype         |
        Equiv           |
        OpenEqn         |
        CloseEqn        |
        Arrow           |
        Colon           |
        Comma           |
        Dot             |
        Lambda          |
        DualOf          |
        Glb             |
        Lub             |
        Int Int         |
        Double Double   |
        Str String
        deriving (Eq,Show)
{- only with
%wrapper "basic"
main = do
  s <- getContents
  print (alexScanTokens s)
-}

}
