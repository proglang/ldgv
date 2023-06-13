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
  "VUnit"                               { tok $ const VUnit }
  "VLabel"                              { tok $ const VLabel }
  "VInt"                                { tok $ const VInt }
  "VDouble"                             { tok $ const VDouble }
  "VString"                             { tok $ const VString }
  "VChan"                               { tok $ const VChan }
  "VChanSerial"                         { tok $ const VChanSerial }
  "VSend"                               { tok $ const VSend }
  "VPair"                               { tok $ const VPair }
  "VType"                               { tok $ const VType }
  "VFunc"                               { tok $ const VFunc }
  "VDynCast"                            { tok $ const VDynCast }
  "VFuncCast"                           { tok $ const VFuncCast }
  "VRec"                                { tok $ const VRec}
  "VNewNatRec"                          { tok $ const VNewNatRec }

  "TUnit"                               { tok $ const TUnit }
  "TInt"                                { tok $ const TInt }
  "TDouble"                             { tok $ const TDouble }
  "TBot"                                { tok $ const TBot }
  "TDyn"                                { tok $ const TDyn }
  "TNat"                                { tok $ const TNat }
  "TString"                             { tok $ const TString }
  "TNatLeq"                             { tok $ const TNatLeq }
  "TNatRec"                             { tok $ const TNatRec }
  "TVar"                                { tok $ const TVar }
  "TAbs"                                { tok $ const TAbs }
  "TName"                               { tok $ const TName }
  "TLab"                                { tok $ const TLab }
  "TFun"                                { tok $ const TFun }
  "TPair"                               { tok $ const TPair }
  "TSend"                               { tok $ const TSend }
  "TRecv"                               { tok $ const TRecv }
  "TCase"                               { tok $ const TCase }
  "TEqn"                                { tok $ const TEqn }
  "TSingle"                             { tok $ const TSingle }

  "ELet"                                { tok $ const ELet }
  "EMath"                               { tok $ const EMath }
  "ELit"                                { tok $ const ELit }
  "ESucc"                               { tok $ const ESucc }
  "ENatRec"                             { tok $ const ENatRec }
  "ENewNatRec"                          { tok $ const ENewNatRec }
  "EVar"                                { tok $ const EVar }
  "ELam"                                { tok $ const ELam }
  "ERec"                                { tok $ const ERec }
  "EApp"                                { tok $ const EApp }
  "EPair"                               { tok $ const EPair }
  "ELetPair"                            { tok $ const ELetPair }
  "EFst"                                { tok $ const EFst }
  "ESnd"                                { tok $ const ESnd }
  "EFork"                               { tok $ const EFork }
  "ENew"                                { tok $ const ENew }
  "ESend"                               { tok $ const ESend }
  "ERecv"                               { tok $ const ERecv }
  "ECase"                               { tok $ const ECase }
  "ECast"                               { tok $ const ECast }

  "MAdd"                                { tok $ const MAdd }
  "MSub"                                { tok $ const MSub }
  "MMul"                                { tok $ const MMul }
  "MDiv"                                { tok $ const MDiv }
  "MNeg"                                { tok $ const MNeg }

  "MOne"                                { tok $ const ValueParsing.ValueTokens.MOne }
  "MMany"                               { tok $ const ValueParsing.ValueTokens.MMany }

  "LInt"                                { tok $ const LInt }
  "LNat"                                { tok $ const LNat }
  "LDouble"                             { tok $ const LDouble }
  "LLab"                                { tok $ const LLab }
  "LUnit"                               { tok $ const LUnit }
  "LString"                             { tok $ const LString }

  "SFuncType"                           { tok $ const SFuncType }

  "GUnit"                               { tok $ const GUnit }
  "GLabel"                              { tok $ const GLabel }
  "GFunc"                               { tok $ const GFunc }
  "GPair"                               { tok $ const GPair }
  "GNat"                                { tok $ const GNat }
  "GNatLeq"                             { tok $ const GNatLeq }
  "GInt"                                { tok $ const GInt }
  "GDouble"                             { tok $ const GDouble }
  "GString"                             { tok $ const GString }

  "PEnv"                                { tok $ const PEnv }
  "PEnvEntry"                           { tok $ const PEnvEntry }
  "SLabelType"                          { tok $ const SLabelType }
  "SStringExpArray"                     { tok $ const SStringExpArray }
  "SStringTypeArray"                    { tok $ const SStringTypeArray }
  "SStringArray"                        { tok $ const SStringArray }
  "SValuesArray"                        { tok $ const SValuesArray }
  "SNetworkConnection"                  { tok $ const SNetworkConnection}
  "SDirectionalConnection"              { tok $ const SDirectionalConnection}
  "SConnected"                          { tok $ const SConnected}

  "NIntroduce"                          { tok $ const NIntroduce }
  "NNewValue"                           { tok $ const NNewValue }
  "NRequestValue"                       { tok $ const NRequestValue }
  "NAcknowledgeValue"                   { tok $ const NAcknowledgeValue }
  "NNewPartnerAddress"                  { tok $ const NNewPartnerAddress }
  "NAcknowledgePartnerAddress"          { tok $ const NAcknowledgePartnerAddress }
  "NDisconnect"                         { tok $ const NDisconnect }
  "NAcknowledgeDisconnect"              { tok $ const NAcknowledgeDisconnect }
  "NRedirect"                           { tok $ const NRedirect }
  "NOkay"                               { tok $ const NOkay }
  "NOkayIntroduce"                      { tok $ const NOkayIntroduce }
  "NWait"                               { tok $ const NWait }
  "NError"                              { tok $ const NError }
  "NConversationMessage"                { tok $ const NConversationMessage }
  "NConversationResponse"               { tok $ const NConversationResponse }
  "NConversationCloseAll"               { tok $ const NConversationCloseAll }

  Double\:[\-]?[0-9]+[\.][0-9]+                  { tok $ Double . read . (drop 7) }
  Int\:[\-]?[0-9]+                              { tok $ Int . read . (drop 4)}
  Integer\:[\-]?[0-9]+                              { tok $ Integer . read . (drop 8)}
  String\:\"[^\"]*\"                            { tok $ String . (drop 7)}
  "Bool:False"                                  { tok $ Bool . ignoreArgument False}
  "Bool:True"                                   { tok $ Bool . ignoreArgument True}
  -- TODO: Add proper String parsing: https://www.jyotirmoy.net/posts/2015-08-17-alex-happy-startcodes.html
  [\=\+\-\*\/\(\)\:\!\?\{\}\[\]\<\>\,]    { tok $ Sym . head }
{
-- The token type:
-- | (Unit, Label, Int, Values of self-declared Data Types), Channels
data Token
  = VUnit
  | VLabel
  | VInt
  | VDouble
  | VString
  | VChan
  | VChanSerial
  | VSend
  | VPair
  | VType
  | VFunc
  | VDynCast
  | VFuncCast
  | VRec
  | VNewNatRec

  | TUnit
  | TInt
  | TDouble
  | TBot
  | TDyn
  | TNat
  | TString
  | TNatLeq
  | TNatRec
  | TVar
  | TAbs
  | TName
  | TLab
  | TFun
  | TPair
  | TSend
  | TRecv
  | TCase
  | TEqn
  | TSingle

  | ELet
  | EMath
  | ELit
  | ESucc
  | ENatRec
  | ENewNatRec
  | EVar
  | ELam
  | ERec
  | EApp
  | EPair
  | ELetPair
  | EFst
  | ESnd
  | EFork
  | ENew
  | ESend
  | ERecv
  | ECase
  | ECast

  | MAdd
  | MSub
  | MMul
  | MDiv
  | MNeg

  | MOne
  | MMany

  | LInt
  | LNat
  | LDouble
  | LLab
  | LUnit
  | LString

  | SFuncType

  | GUnit
  | GLabel
  | GFunc
  | GPair
  | GNat
  | GNatLeq
  | GInt
  | GDouble
  | GString

  | PEnv
  | PEnvEntry

  | SLabelType

  | SStringExpArray
  | SStringTypeArray
  | SStringArray
  | SValuesArray

  | SNetworkConnection
  | SDirectionalConnection
  | SConnected

  | NIntroduce
  | NNewValue
  | NRequestValue
  | NAcknowledgeValue
  | NNewPartnerAddress
  | NAcknowledgePartnerAddress
  | NDisconnect
  | NAcknowledgeDisconnect
  | NRedirect
  | NOkay
  | NOkayIntroduce
  | NWait
  | NError
  | NConversationMessage
  | NConversationResponse
  | NConversationCloseAll

  | String String
  | Int Int
  | Integer Integer
  | Double Double
  | Bool Bool

  | Sym Char
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


scanner str = runAlex str $ loop []
  where
    loop i = do 
      tok <- alexMonadScan
      if (tokVal tok) == EOF then return i else loop $! (i++[(tokVal tok)])

ignoreArgument a b = a 
}
