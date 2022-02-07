{
{-# LANGUAGE BlockArguments #-}
module Parsing.Tokens
  ( -- * Tokens
    Token(..)
  , AlexPosn(..)
  , T(..)

    -- * Alex monad
  , Alex
  , runAlex
  , alexMonadScan
  , alexError
  ) where

import Kinds
import Text.Read (readMaybe)
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

  -- for Binary Session Types; obsolete for Label Dependent ones
  select                                { tok $ const Select }
  rcase                                 { tok $ const Rcase }
  close                                 { tok $ const Close }
  wait                                  { tok $ const Wait }

  expect                                { tok $ const Expect }
  $digit+ "." $digit+                   { tok $ Double . read }
  $digit+                               { tok $ Int . read }
  Bot                                   { tok $ const TBot }
  Unit                                  { tok $ const TUnit }
  Int                                   { tok $ const TInt }
  natrec                                { tok $ const NatRec }
  new_natrec                            { tok $ const NewNatRec }
  Nat                                   { tok $ const TNat }
  Double                                { tok $ const TDouble }
  dualof                                { tok $ const DualOf }
  "_|_"                                 { tok $ const TBot }
  "()"                                  { tok $ const Unit }
  "->"                                  { tok $ const Arrow }
  "=>"                                  { tok $ const DoubleArrow }
  "{{"                                  { tok $ const OpenEqn }
  "}}"                                  { tok $ const CloseEqn }
  "<:"                                  { tok $ const Subtype }
  "=:"                                  { tok $ const Equiv }
  ":"                                   { tok $ const Colon }
  ","                                   { tok $ const Comma }
  "."                                   { tok $ const Dot }
  fn | 𝜆                                { tok $ const Lambda }
  [\=\+\-\*\/\(\)\:\!\?\{\}\[\]\<\>]    { tok $ Sym . head }
  "'" [$alpha $digit]+                  { tok $ Lab }
  "~" $alpha+                           { tokKind }
  $lower [$alpha $digit \_ \']*         { tok $ Var }
  $upper [$alpha $digit \_ \']*         { tok $ TID }

{
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

        -- for Binary Session Types; obsolete for Label Dependent ones
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
        NatRec          |
        NewNatRec       |
        Subtype         |
        Equiv           |
        OpenEqn         |
        CloseEqn        |
        Arrow           |
        DoubleArrow     |
        Colon           |
        Comma           |
        Dot             |
        Lambda          |
        DualOf          |
        Int Int         |
        Double Double   |
        EOF
        deriving (Eq,Show)

data T = T { tokPos :: AlexPosn, tokVal :: !Token }

alexEOF :: Alex T
alexEOF = do
  (pos, _, _, _) <- alexGetInput
  pure $ T pos EOF

tok :: (String -> Token) -> AlexAction T
tok f = tok' (Right . f)

tok' :: (String -> Either String Token) -> AlexAction T
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
    Right tok -> pure $ T pos tok

tokKind :: AlexAction T
tokKind = tok' \k ->
  maybe (Left $ "invalid kind " ++ k) (Right . Kind)
    $ readMaybe
    $ ('K':)    -- Subsitutes the initial '~' with 'K'
    $ tail k
}
