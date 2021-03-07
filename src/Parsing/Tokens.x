{
module Parsing.Tokens
  ( Token(..)
  , T(..)
  , tokVal
  , AlexPosn(..)
  , runAlex
  , alexMonadScan
  ) where
import Kinds
}

%wrapper "monad"

$digit = 0-9                    -- digits
$alpha = [a-zA-Z]               -- alphabetic characters
$lower = [a-z]
$upper = [A-Z]

tokens :-

  $white+                               ;
  "--".*                                ;
  assume                                { tok $ const Assume }
  case                                  { tok $ const Case }
  type                                  { tok $ const Type }
  let                                   { tok $ const Let }
  rec                                   { tok $ const Rec }
  fst                                   { tok $ const Fst }
  snd                                   { tok $ const Snd }
  in                                    { tok $ const In }
  of                                    { tok $ const Of }
  val                                   { tok $ const Val }
  fork                                  { tok $ const Fork }
  new                                   { tok $ const New }
  send                                  { tok $ const Send }
  recv                                  { tok $ const Recv }
  select                                { tok $ const Select }
  rcase                                 { tok $ const Rcase }
  close                                 { tok $ const Close }
  wait                                  { tok $ const Wait }
  expect                                { tok $ const Expect }
  $digit+                               { tok $ Int . read }
  Bot                                   { tok $ const TBot }
  Unit                                  { tok $ const TUnit }
  Int                                   { tok $ const TInt }
  natrec                                { tok $ const NatRec } 
  Nat                                   { tok $ const TNat }
  dualof                                { tok $ const DualOf }
  "_|_"                                 { tok $ const TBot }
  "/\"                                  { tok $ const Glb }
  "\/"                                  { tok $ const Lub }
  "()"                                  { tok $ const Unit }
  "->"                                  { tok $ const Arrow }
  "{{"                                  { tok $ const OpenEqn }
  "}}"                                  { tok $ const CloseEqn }
  "<:"                                  { tok $ const Subtype }
  "=:"                                  { tok $ const Equiv }
  ":"                                   { tok $ const Colon }
  ","                                   { tok $ const Comma }
  "."                                   { tok $ const Dot }
  fn                                    { tok $ const Lambda }
  [\=\+\-\*\/\(\)\:\!\?\{\}\[\]\<\>]    { tok $ Sym . head }
  "'" [$alpha $digit]+                  { tok $ Lab }
  "~" $alpha+                           { tok $ Kind . read . ('K':) . tail }
  $lower [$alpha $digit \_ \']*         { tok $ Var }
  $upper [$alpha $digit \_ \']*         { tok $ TID }

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

data T = T AlexPosn !Token

alexEOF :: Alex (Maybe T)
alexEOF = return Nothing

tok :: (String -> Token) -> AlexAction (Maybe T)
tok f = tok' (Right . f)

tok' :: (String -> Either String Token) -> AlexAction (Maybe T)
tok' f (pos@(AlexPn _ line column), _, _, inp) len = do
  let inp' = take len inp
  case f inp' of
    Left err -> alexError $ mconcat
      [ "lexical error at line "
      , show line
      , ", column "
      , show column
      , if null err then "" else (": " ++ err)
      ]
    Right tok -> pure $ Just $ T pos tok

tokVal :: T -> Token
tokVal (T _ tok) = tok
}
