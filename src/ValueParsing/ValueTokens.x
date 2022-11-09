{
{-# LANGUAGE BlockArguments #-}
module ValueParsing.ValueTokens
  ( -- * Tokens
    Token(..)
  , AlexPosn(..)
  , T(..)

    -- * Alex monad
  , Alex
  , runAlex
  , alexMonadScan
  , alexError
  , scanner
  ) where

import Kinds
import Text.Read (readMaybe)
}

%wrapper "monad"
-- %wrapper "monadUserState"
-- %wrapper "basic"

$digit = 0-9                    -- digits
$alpha = [a-zA-Z]               -- alphabetic characters
$lower = [a-z]
$upper = [A-Z]

tokens :-

  $white+                               ;
  VUnit                               { tok $ const VUnitN }
  VLabel                              { tok $ const VLabelN}
  VInt                                { tok $ const VIntN}
  VString                             { tok $ const VStringN}
  VPair                               { tok $ const VPairN}
  VSend                               { tok $ const VSendN}
  $digit+ "." $digit+                   { tok $ Double . read }
  $digit+                               { tok $ Int . read }
  \"\"[^\"]*\"\"                            { tok $ String }
  "<"                                   { tok $ const LesserN }
  ">"                                   { tok $ const GreaterN }
  ","                                   { tok $ const CommaN }
  "("                                   { tok $ const ParOpenN }
  ")"                                   { tok $ const ParCloseN }
  $alpha [$alpha $digit \_ \']*         { tok $ Label }

{
-- The token type:
-- | (Unit, Label, Int, Values of self-declared Data Types), Channels
data Token
  = VUnitN
  | VLabelN
  | VIntN
  | VDoubleN
  | VStringN
  | VSendN
  | VPairN
  | Label String
  | String String
  | Int Int
  | Double Double
  | GreaterN
  | LesserN
  | CommaN
  | ParOpenN
  | ParCloseN
  | EOF
  deriving (Eq, Show)

data T = T { tokPos :: AlexPosn, tokVal :: !Token }
  deriving (Eq, Show)

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

{-
tokKind :: AlexAction T
tokKind = tok' \k ->
  maybe (Left $ "invalid kind " ++ k) (Right . Kind)
    $ readMaybe
    $ ('K':)    -- Subsitutes the initial '~' with 'K'
    $ tail k

    -}

-- runAlexScan :: String -> Either ParseError AlexUserState
{-scanner str = runAlex str $ do
  let loop i = do tok <- alexMonadScan
                  if (tokVal tok) == EOF then return i
			else do let i' = i+1 in i' `seq` loop i'
  loop 0-}

{-scanner str = runAlex str $ do
  let loop i = do tok <- alexMonadScan; 
		  if (tokVal tok) == EOF
			then return i
			else do loop $! (i+1)
  loop 0-}

scanner str = runAlex str $ do
  let loop i = do tok <- alexMonadScan; 
		  if (tokVal tok) == EOF
			then return i
			else do loop $! (i++[(tokVal tok)])
  loop []
}

-- https://gist.github.com/m1dnight/126d6b500175c2c286e3804584e5c4ce