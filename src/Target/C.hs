{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}
module Target.C where

import Control.Lens
import Control.Monad.Except
import Control.Monad.RWS.Strict
import Data.ByteString.Builder (Builder)
import Data.Coerce
import Data.Map (Map)
import Data.Semigroup
import Syntax
import qualified Data.ByteString.Builder as B
import qualified Data.List as List
import Control.Monad.State.Strict
import Data.Foldable

-- | Represents values lifted into the @LDST_t@ type.
newtype CExp = CExp { unCExp :: Builder }

data Location = Stack | Heap

data CVar x where
  StackVar :: !Builder -> CVar 'Stack
  HeapVar :: !Builder -> CVar 'Heap

newtype CStmt = CStmt { unCStmt :: Builder }

instance Semigroup CStmt where
  CStmt a <> CStmt b = CStmt (a <> "\n" <> b)

data Tag a where
  TagInt :: Tag Int
  TagLabel :: Tag String
  TagPair :: Tag (CExp, CExp)

data Info = Info
  { _infoBindings :: !(Map Ident (CVar 'Stack))
    -- ^ Mapping from bound variables to the corresponding identifiers in the
    -- generated C code.

  , _infoNameHint :: !Builder
    -- ^ Prepended to all fresh variables, helps with understandability of the
    -- generated C code and tracking to which expression the variables belong.

  , _infoIndent :: !Int
    -- ^ Current indent level.
  }

makeLenses ''Info

type GenM = ExceptT String (RWS Info (Maybe CStmt) Word)

generate :: [Decl] -> Builder
generate _ = error "generate: not implemented"

generateExp :: Exp -> GenM CExp
generateExp = \case
  Let v a b -> do
    scoped v (generateExp a) (const $ generateExp b)
  Plus a b -> mathOp '+' a b
  Minus a b -> mathOp '-' a b
  Times a b -> mathOp '*' a b
  Div a b -> mathOp '/' a b
  Negate e -> do
    e' <- stmt =<< generateExp e
    pure $ liftValue TagInt $ B.char7 '-' <> access TagInt e'
  Int i -> do
    mkValue TagInt i
  Nat i -> do
    mkValue TagInt i
  Succ e -> do
    e' <- stmt =<< generateExp e
    pure $ liftValue TagInt $ access TagInt e' <> "+ 1"
  NatRec e e1 l_c l_c3 l_c4 t e6 -> undefined
  Var v -> do
    v' <- view (infoBindings . at v)
    maybe (doesNotExist v) (pure . varToExp) v'
  Unit -> undefined
  Lab lbl -> do
    mkValue TagLabel lbl
  Lam m l_c t e -> undefined
  Rec l_c l_c1 t t3 e -> undefined
  App e e1 -> undefined
  Pair _ idnA a b -> do
    scoped idnA (generateExp a) \a' -> do
      b' <- generateExp b
      mkValue TagPair (varToExp a', b')
  LetPair idnFst idnSnd pairExp body -> do
    -- Evaluate pairExp first.
    scoped (idnFst ++ "_" ++ idnSnd) (generateExp pairExp) \pairVar ->
      let (valFst, valSnd) = accessPair pairVar
       in scoped idnFst (pure (varToExp valFst)) \_ ->
          scoped idnSnd (pure (varToExp valSnd)) \_ ->
            generateExp body
  Fst e -> do
    varToExp . fst . accessPair <$> (stmt =<< generateExp e)
  Snd e -> do
    varToExp . snd . accessPair <$> (stmt =<< generateExp e)
  Fork e -> undefined
  New t -> undefined
  Send e -> undefined
  Recv e -> undefined

  Case e cs -> do
    -- TODO: It is possible to arrange the comparisons to find the correct
    -- branch in O(log n) steps.
    --
    -- TODO: Should we assume that the matching branch always exists? Or check
    -- all branches and panic, in case none matches?
    label <- access TagLabel <$> (stmt =<< generateExp e)
    result <- fresh
    tellStmt $ mconcat [ ctype, B.char7 ' ', result, B.char7 ';' ]
    let buildBranch :: (String, Exp) -> StateT Builder GenM ()
        buildBranch (branchLabel, branchExp) = do
          ifB <- get <* put "else if"
          let cmpExp = callExp funStrcmp [label, labelForC branchLabel] <> " == 0"
          lift $ tellStmt $ callExp ifB [cmpExp] <> " {"
          lift $ local (infoIndent +~ 1) do
            e' <- generateExp branchExp
            tellStmt $ bunwords [ result, B.char7 '=', unCExp e' <> B.char7 ';' ]
          lift $ tellStmt "}"
    evalStateT (traverse_ buildBranch cs) ("if" :: Builder)
    pure $ varToExp $ StackVar result

doesNotExist :: MonadError String m => Ident -> m a
doesNotExist idn = throwError $ "variable " ++ show idn ++ " does not exist"

scoped :: Ident -> GenM CExp -> (CVar 'Stack -> GenM b) -> GenM b
scoped idn val body = do
  var <- local (infoNameHint .~ identForC idn) $ stmt =<< val
  local (infoBindings . at idn ?~ var) $ body var

mathOp :: Char -> Exp -> Exp -> GenM CExp
mathOp c a b = do
  a' <- stmt =<< generateExp a
  b' <- stmt =<< generateExp b
  pure
    $ liftValue TagInt
    $ bunwords [ access TagInt a', B.char7 c, access TagInt b' ]


fresh :: GenM Builder
fresh = do
  n <- get <* modify' (+1)
  hint <- view infoNameHint
  pure $ hint <> B.char7 '_' <> B.wordHex n

stmt :: CExp -> GenM (CVar 'Stack)
stmt e = do
  var <- fresh
  tellStmt $ bunwords
    [ ctype
    , var
    , B.char7 '='
    , unCExp e <> B.char7 ';'
    ]
  pure (StackVar var)

clone :: CExp -> GenM (CVar 'Heap)
clone e = do
  var <- fresh
  tellStmt $ bunwords
    [ ctype
    , B.char7 '*' <> var
    , B.char7 '='
    , callExp funMalloc [sizeofCtype] <> B.char7 ';'
    ]
  tellStmt $ callExp funMemcpy [ var, takeAddress e, sizeofCtype ]
  pure (HeapVar var)

callExp :: Builder -> [Builder] -> Builder
callExp f args =
  let args' = mconcat $ List.intersperse ", " $ coerce args
   in f <> B.char7 '(' <> args' <> B.char7 ')'

funMalloc :: Builder
funMalloc = "malloc"

funMemcpy :: Builder
funMemcpy = "memcpy"

funStrcmp :: Builder
funStrcmp = "strcmp"

sizeofCtype :: Builder
sizeofCtype = callExp "sizeof" [ctype]

takeAddress :: CExp -> Builder
takeAddress (CExp e) = B.char7 '&' <> e
-- It is ok to take the address here of the expression in `e`, even if it
-- evaluates to a temporary object.


tellStmt :: Builder -> GenM ()
tellStmt s = do
  lvl <- view infoIndent
  let !indent = stimes (lvl * 2) (B.char7 ' ')
  tell $ Just $! CStmt $ indent <> s


-- | The type of all LDST values in the generated C code.
ctype :: Builder
ctype = "LDST_t"

mkValue :: Tag a -> a -> GenM CExp
mkValue tag a = liftValue tag <$> case tag of
  TagInt -> pure $ B.intDec a
  TagLabel -> pure $ labelForC a
  TagPair -> do
    let (x, y) = a
    CExp x' <- varToExp <$> clone x
    CExp y' <- varToExp <$> clone y
    pure $ "{ " <> x' <> ", " <> y' <> " }"

liftValue :: Tag a -> Builder -> CExp
liftValue tag a = CExp $ bunwords
  [ "(" <> ctype <> "){"
  , tagAccessor tag
  , B.char7 '='
  , a
  , B.char7 '}'
  ]

access :: Tag a -> CVar x -> Builder
access tag v = unCExp (varToExp v) <> tagAccessor tag

accessPair :: CVar x -> (CVar 'Heap, CVar 'Heap)
accessPair v =
  let b = access TagPair v
   in (HeapVar $ b <> "[0]", HeapVar $ b <> "[1]")

varToExp :: CVar x -> CExp
varToExp = CExp . \case
  StackVar v -> v
  HeapVar v -> B.char7 '(' <> B.char7 '*' <> v <> B.char7 ')'

tagAccessor :: Tag a -> Builder
tagAccessor = \case
  TagInt -> ".val_int"
  TagLabel -> ".val_label"
  TagPair -> ".val_pair"

tagType :: Tag a -> Builder -> Builder
tagType tag n = case tag of
  TagInt -> "int " <> n
  TagLabel -> "const char *" <> n
  TagPair -> mconcat [ "union ", ctype, " *", n, "[2]" ]

-- | Concatenate a list of builders using a single space character.
bunwords :: [Builder] -> Builder
bunwords = mconcat . List.intersperse (B.char7 ' ')

-- | Escapes an LDST identifier for use in C code. It is based on the
--   z-encoding used in GHC.
--
--   * underscores are replaced by @z_@, this is to protect against accidental
--     shadowing as single underscores are used in generated identifiers.
--
--   * primes/single quotes are replaced by @zq@
--
--   * @z@ is replaced by @zz@
identForC :: Ident -> Builder
identForC = foldMap \case
  '_'  -> "z_"
  '\'' -> "zq"
  'z'  -> "zz"
  -- In theory c can only be an ASCII character, but better safe than sorry.
  c -> B.charUtf8 c

labelForC :: String -> Builder
labelForC lbl =
  -- The LDST grammar only allows characters which are valid in C strings.
  B.char7 '"' <> B.stringUtf8 lbl <> B.char7 '"'
