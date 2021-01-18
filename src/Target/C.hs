{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}
module Target.C (generate) where

import Control.Lens
import Control.Monad.Except
import Control.Monad.RWS.Strict
import Control.Monad.State.Strict
import Data.Bifunctor
import Data.ByteString.Builder (Builder)
import Data.Foldable
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Semigroup
import Data.Set (Set)
import Kinds
import Singletons
import Syntax
import Validation
import qualified Control.Foldl as L
import qualified Data.ByteString.Builder as B
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- | Represents values lifted into the @LDST_t@ type.
newtype CExp = CExp { unCExp :: Builder }

data Location = Stack | Heap

data SLocation x where
  SStack :: SLocation 'Stack
  SHeap :: SLocation 'Heap

type instance The Location = SLocation

instance Known 'Stack where sing = SStack
instance Known 'Heap where sing = SHeap

data CVar x where
  StackVar :: !Builder -> CVar 'Stack
  HeapVar :: !Builder -> CVar 'Heap

newtype CStmt = CStmt Builder
  deriving newtype (Semigroup, Monoid)

data Tag a where
  TagInt :: Tag Int
  TagLabel :: Tag String
  TagPair :: Tag (CExp, CExp)
  TagLam :: Tag (Builder, Set Ident)

data Info = Info
  { _infoBindings :: !(Map Ident (CVar 'Stack))
    -- ^ Mapping from bound variables to the corresponding identifiers in the
    -- generated C code.

  , _infoNameHint :: !Builder
    -- ^ Prepended to all fresh variables, helps with understandability of the
    -- generated C code and tracking to which expression the variables belong.

  , _infoFuncHint :: !Builder
    -- ^ Prepended to all functions originating from splitting lambdas and
    -- continuations out of their enclosing function. This is necessary to be
    -- unique per function, otherwise the generated function names might clash.

  , _infoIndent :: !Int
    -- ^ Current indent level.
  }

makeLenses ''Info

type GenM = RWST Info CStmt Word (Either String)

generate :: [Decl] -> Either String Builder
generate = first (List.intercalate "\n" . toList) . validationToEither . foldMap \case
  DFun name args body _ -> generateFunction name args body
  _ -> mempty

generateFunction
  :: Ident
  -> [(Multiplicity, Ident, Type)]
  -> Exp
  -> Validation (NonEmpty String) Builder
generateFunction name args body =
  let (argsBuilder, argsBindings) = L.fold (view _2 `L.premap`  goArgId) args

      goArgId = (,)
        <$> (\idn -> ctype <> B.char7 ' ' <> identForC idn) `L.premap` L.list
        <*> (\idn -> (idn, StackVar (identForC idn))) `L.premap` L.map

      funHeader = bunwords
        [ ctype
        , callExp (functionForC name) argsBuilder
        , "{\n"
        ]

      funClose = "}\n\n"

      addContext err =
        "in function ‘" ++ name ++ "’:\n" ++ err

      genBody = do
        result <- generateExp body
        tellStmt $ mconcat
          [ "return "
          , unCExp result
          , B.char7 ';'
          ]

      completeFunction (CStmt body') = mconcat
        [ funHeader
        , body'
        , funClose
        ]

      info = Info
        { _infoBindings = argsBindings -- TODO: include other functions
        , _infoNameHint = identForC name
        , _infoFuncHint = functionForC name
        , _infoIndent = 1
        }

   in eitherToValidation
        $ bimap (pure . addContext) (completeFunction . snd)
        $ evalRWST genBody info 0


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
  NatRec{} -> undefined
  Var v -> do
    v' <- view (infoBindings . at v)
    maybe (doesNotExist v) (pure . varToExp) v'
  Unit -> undefined
  Lab lbl -> do
    mkValue TagLabel lbl
  Lam _ argId _ body -> do
    -- TODO: Generate actual code for the lambda.
    name <- fresh (Just "lam")
    mkValue TagLam (name, Set.fromList (filter (/= argId) (fv body)))
  Rec{} -> undefined
  App funExp argExp -> do
    -- TODO: Directly call statically known top level functions.
    lam <- stmt =<< generateExp funExp
    arg <- generateExp argExp
    let (fun, closure) = accessLambda lam
    pure $ CExp $ callExp fun [closure, unCExp arg]
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
    result <- declareFresh @'Stack ctype Nothing
    let buildBranch :: (String, Exp) -> StateT Builder GenM ()
        buildBranch (branchLabel, branchExp) = do
          ifB <- get <* put "else if "
          let cmpExp = callExp funStrcmp [label, labelForC branchLabel] <> " == 0"
          lift $ tellStmt $ callExp ifB [cmpExp] <> " {"
          lift $ local (infoIndent +~ 1) do
            e' <- generateExp branchExp
            tellStmt $ bunwords [ unCExp (varToExp result), B.char7 '=', unCExp e' <> B.char7 ';' ]
          lift $ tellStmt "}"
    evalStateT (traverse_ buildBranch cs) ("if " :: Builder)
    pure $ varToExp result

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

-- | Generates a guaranteed fresh name for the current function. The returned
-- identifier is suitable for use in C code provided that 'infoNameHint' and
-- 'infoFuncHint' are never an invalid prefix.
--
-- If the first argument is @Just /funKind/@ the name is guaranteed to be fresh
-- for the whole module and @fresh@ uses 'infoFuncHint' instead of
-- 'infoNameHint' with @/funKind/@ appended before the unique id.
fresh :: Maybe Builder -> GenM Builder
fresh funKind = do
  n <- get <* modify' (+1)
  hint <- case funKind of
    Nothing -> view infoNameHint
    Just fk -> (\h -> h <> B.char7 '_' <> fk) <$> view infoFuncHint
  pure $ hint <> B.char7 '_' <> B.wordHex n

declareFresh :: forall x. Known x => Builder -> Maybe Builder -> GenM (CVar x)
declareFresh varType initExp = do
  name <- fresh Nothing
  tellStmt $ mconcat
    [ varType
    , case sing @_ @x of
        SStack -> " "
        SHeap -> " *"
    , name
    , fold $ (" = " <>) <$> initExp
    , B.char7 ';'
    ]
  pure case sing @_ @x of
         SStack -> StackVar name
         SHeap -> HeapVar name

-- | Writes the result of the given expression into a fresh variable.
stmt :: CExp -> GenM (CVar 'Stack)
stmt = declareFresh ctype . Just . unCExp

-- | Clones the result of the given expression into a fresh variable which
-- lives on the heap instead of the stack.
clone :: CExp -> GenM (CVar 'Heap)
clone e = do
  var <- declareFresh ctype $ Just $ callExp funMalloc [sizeofCtype]
  tellStmt $ callExp funMemcpy [ varName var, takeAddress e, sizeofCtype ]
  pure var

-- | Glues the parts together to yield something looking like a function call.
-- It is also used to generate function headers and control structures.
callExp :: Builder -> [Builder] -> Builder
callExp f args = f <> parens (intercalate ", " args)

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
  tell $ CStmt $ indent <> s <> B.char7 '\n'


-- | The type of all LDST values in the generated C code.
--
-- @
-- union LDST_t {
--   int val_int;
--   const char *val_label;
--   union LDST_t *val_pair[2];
--   struct LDST_lam_t val_lam;
-- };
-- @
ctype :: Builder
ctype = "union LDST_t"

-- | The type of lambdas and closures in the generated C code.
--
-- @
-- struct LDST_lam_t {
--   LDST_t (*lam_fp)(LDST_t *closure, LDST_t arg);
--   LDST_t *lam_closure;
-- };
-- @
lambdaType :: Builder
lambdaType = "struct LDST_lam_t"

mkValue :: Tag a -> a -> GenM CExp
mkValue tag a = liftValue tag <$> case tag of
  TagInt -> pure $ B.intDec a
  TagLabel -> pure $ labelForC a
  TagPair -> do
    let (x, y) = a
    CExp x' <- varToExp <$> clone x
    CExp y' <- varToExp <$> clone y
    pure $ braceList Nothing [x', y']
  TagLam -> do
    let (fun, capturedIds) = a
    capturedVars <- flip Map.restrictKeys capturedIds <$> view infoBindings
    let varCount = length capturedVars
    closure <- if varCount == 0
      then do
        -- `malloc` of size 0 is not allowed, use a NULL closure instead.
        pure (B.char7 '0')
      else do
        let size = B.intDec varCount <> " * " <> sizeofCtype
        closure <- declareFresh @'Heap ctype $ Just $ callExp funMalloc [size]
        tellStmt $ callExp funMemcpy
          [ varName closure
          , braceList (Just (ctype <> "[]")) (unCExp . varToExp . snd <$> Map.toAscList capturedVars)
          ]
        pure $ varName closure
    pure $ braceList Nothing [fun, closure]

liftValue :: Tag a -> Builder -> CExp
liftValue tag a = CExp
  $ braceList (Just ctype)
  $ pure
  $ bunwords
      [ tagAccessor tag
      , B.char7 '='
      , a
      ]

braceList :: Maybe Builder -> [Builder] -> Builder
braceList annot bs =
  foldMap parens annot <> braces (intercalate ", " bs)

access :: Tag a -> CVar x -> Builder
access tag v = unCExp (varToExp v) <> tagAccessor tag

accessPair :: CVar x -> (CVar 'Heap, CVar 'Heap)
accessPair v =
  let b = access TagPair v
   in (HeapVar $ b <> "[0]", HeapVar $ b <> "[1]")

accessLambda :: CVar x -> (Builder, Builder)
accessLambda v =
  let b = access TagLam v
   in (b <> ".lam_fp", b <> ".lam_closure")

varToExp :: CVar x -> CExp
varToExp = CExp . \case
  StackVar v -> v
  HeapVar v -> parens (B.char7 '*' <> v)

varName :: CVar x -> Builder
varName = \case
  StackVar n -> n
  HeapVar n -> n

tagAccessor :: Tag a -> Builder
tagAccessor = \case
  TagInt   -> ".val_int"
  TagLabel -> ".val_label"
  TagPair  -> ".val_pair"
  TagLam   -> ".val_lam"

_tagType :: Tag a -> Builder -> Builder
_tagType tag n = case tag of
  TagInt   -> "int " <> n
  TagLabel -> "const char *" <> n
  TagPair  -> mconcat [ "union ", ctype, " *", n, "[2]" ]
  TagLam   -> lambdaType

-- | Concatenate a list of builders using a single space character.
bunwords :: [Builder] -> Builder
bunwords = intercalate (B.char7 ' ')

-- | @"Data.List".'List.intercalate'@ generalized to arbitrary monoids.
--
-- >>> intercalate "a" ["x", "y", "z"]
-- "xayaz"
-- >>> getDual $ intercalate (Dual "a") (Dual <$> ["x", "y", "z"])
-- "zayax"
intercalate :: Monoid a => a -> [a] -> a
intercalate a = mconcat . List.intersperse a

-- | @surround l r a@ adds @l@ to the left of @a@ and @r@ to the right.
--
-- >>> surround "(" ")" "abc"
-- "(abc)"
surround :: Semigroup a => a -> a -> a -> a
surround l r a = l <> a <> r

-- | Wraps the given builder in parentheses.
--
-- @
-- parens b === surround "(" ")" b
-- @
parens :: Builder -> Builder
parens = surround (B.char7 '(') (B.char7 ')')

-- | Wraps the given builder in parentheses.
--
-- @
-- braces b === surround "{" "}" b
-- @
braces :: Builder -> Builder
braces = surround (B.char7 '{') (B.char7 '}')

-- | Escapes an LDST identifier for use in C code. It is based on the
--   z-encoding used in GHC.
--
--   * underscores are replaced by @z_@, this is to protect against accidental
--     shadowing as single underscores are used in generated identifiers.
--
--   * primes/single quotes are replaced by @zq@
--
--   * @z@ is replaced by @zz@ to make the transformation bijective.
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

-- | Turn an identifier into a function name suitable in the generated C code.
-- It uses the encoding from 'identForC' and prepends @"ldst__"@
functionForC :: Ident -> Builder
functionForC idn = "ldst__" <> identForC idn
