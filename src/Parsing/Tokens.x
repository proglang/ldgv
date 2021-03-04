{
module Parsing.Tokens where
import Kinds
}

%wrapper "basic"

$digit = 0-9                    -- digits
$alpha = [a-zA-Z]               -- alphabetic characters
$lower = [a-z]
$upper = [A-Z]

tokens :-

  $white+                               ;
  "--".*                                ;
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
  $digit+                               { Int . read }
  Bot                                   { const TBot }
  Unit                                  { const TUnit }
  Int                                   { const TInt }
  natrec                                { const NatRec } 
  Nat                                   { const TNat }
  dualof                                { const DualOf }
  "_|_"                                 { const TBot }
  "/\"                                  { const Glb }
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
  fn                                    { const Lambda }
  [\=\+\-\*\/\(\)\:\!\?\{\}\[\]\<\>]    { Sym . head }
  "'" [$alpha $digit]*                  { Lab }
  "~" $alpha+                           { Kind . read . ('K':) . tail }
  $lower [$alpha $digit \_ \']*         { Var }
  $upper [$alpha $digit \_ \']*         { TID }

{
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
        Kind Kind      |
        Lab String      |
        Var String      |
        TID String      |
        Unit            |
        TBot            |
        TUnit           |
        TInt            |
        TNat            |
        NatRec          |
        Subtype         |
        Equiv         |
        OpenEqn         |
        CloseEqn        |
        Arrow           |
        Colon           |
        Comma           |
        Dot             |
        Lambda          |
        DualOf          |
        Glb          |
        Lub          |
        Int Int
        deriving (Eq,Show)

main = do
  s <- getContents
  print (alexScanTokens s)
}
