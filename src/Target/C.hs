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
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}
module Target.C (generate) where

import Control.Lens
import Control.Monad.Except
import Control.Monad.RWS.Strict
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Data.Bifunctor
import Data.ByteString.Builder (Builder)
import Data.Foldable
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Semigroup
import Data.Set (Set)
import Data.String
import Data.Version
import Singletons
import Syntax
import Validation
import qualified Control.Foldl as L
import qualified Data.ByteString.Builder as B
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Paths_ldgv

-- | Represents values lifted into the @LDST_t@ type.
newtype CExp = CExp { unCExp :: Builder }
-- TODO: By using an ADT to differentiate what the expression might represent
-- we could generate more idiomatic code. This isn't terribly necessary though,
-- the common C compilers are able to understand our intentions quite well.

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

newtype CStmt = CStmt { unCStmt :: Builder }
  deriving newtype (Semigroup, Monoid)

data Tag a where
  TagInt :: Tag Int
  TagLabel :: Tag String
  TagPair :: Tag (CExp, CExp)
  TagLam :: Tag (Builder, Closure)

data Function = Function
  { funName    :: !Builder
  , funHint    :: !Builder
    -- ^ See '_infoNameHint'.
  , funArgs    :: !(Maybe Ident)
    -- ^ The functions parameters, not including any potential closure arguments.
  , funBody    :: !Exp
  , funClosure :: !(Maybe [Ident])
    -- ^ @Just vars@ if the first parameter of the function should be a closure
    -- argument.
  , funInternal :: !Bool
    -- ^ @True@ if this function is only used internally and should get
    -- @static@ linkage.
    --
    -- Internal functions originate from lambda expressions while the nullary
    -- top level functions correspond to non-internal functions.
  }

data Closure = Closure
  { closureVars :: ![Ident]
    -- ^ List of captured identifiers. The order corresponds to the order in
    -- the C code in 'closureExpr'.
  , closureExpr :: !Builder
    -- ^ An expression of type @union LDST_t*@.
  }

type Env = Map Ident (CVar 'Stack)

data Info = Info
  { _infoBindings :: !Env
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

type GenM =
  RWST Info CStmt Word
    (QueueT Function (Either String))

newtype QueueT q m a = QueueT { unQueueT :: StateT [q] m a }
  deriving newtype (Functor, Applicative, Monad, MonadError e, MonadWriter w)

evalQueueT :: Monad m => [q] -> QueueT q m a -> m a
evalQueueT qs = flip evalStateT qs . unQueueT

pushQ :: Monad m => q -> QueueT q m ()
pushQ q = QueueT $ modify (q :)

popQ :: Monad m => QueueT q m (Maybe q)
popQ = QueueT do
  qs <- get
  case qs of
    [] -> pure Nothing
    q:qs' -> Just q <$ put qs'

generate :: Maybe Ident -> [Decl] -> Either String Builder
generate _entryPoint = bimap concatErrors (uncurry joinParts) . validationToEither . foldMap \case
  DFun name args body _ -> do
    -- Curry the function.
    let lambdaBody = foldr (\(m, idn, ty) -> Lam m idn ty) body args

    let root = Function
          { funName = functionForC name
          , funHint = identForC name
          , funArgs = Nothing
          , funBody = lambdaBody
          , funClosure = Nothing
          , funInternal = False
          }

    let addContext err =
          "in function ‘" ++ name ++ "’:\n" ++ err

    eitherToValidation
      $ first (pure . addContext)
      $ generateFunction root

  _ ->
    -- Nothing to generate for this kind of top level thingy.
    mempty

  where
    joinParts :: Builder -> Builder -> Builder
    joinParts decls defs = bunlines
      [ header
      , ""
      , "// Generated code - forward declarations"
      , decls
      , "// Generated code - function definitions"
      , defs
      ]

    concatErrors :: NonEmpty String -> String
    concatErrors = intercalate "\n\n" . toList

header :: Builder
header = bunlines
  [ "//"
  , "// Generated by ldgv v" <> fromString (showVersion Paths_ldgv.version)
  , "//"
  , ""
  , "#include <stdlib.h>    // malloc"
  , "#include <string.h>    // strcmp"
  , ""
  , "// Forward declarations"
  , unCStmt $ terminate ctype
  , unCStmt $ terminate lambdaType
  , ""
    -- The definition for the lambda type has to come first, because it is a
    -- non-pointer field in the main type.
  , "// Type declarations"
  , lambdaType <> " {"
  ,   lamTag ctype (callExp "(*lam_fp)" [ctypePtr, ctype])
  ,   lamTag ctypePtr "lam_closure"
  , "};"
  , ctype <> " {"
  ,   tag TagInt
  ,   tag TagLabel
  ,   tag TagLam
  ,   tag TagPair
  , "};"
  ]
  where
    tag :: forall a. Tag a -> Builder
    tag t = unCStmt $ terminate $ "  " <> tagType t (tagAccessor t)
    lamTag a b = unCStmt $ terminate $ "  " <> bunwords [a, b]
    ctypePtr = ctype <> B.char7 '*'

-- | Buids a function signature and an 'Env' binding the arguments to the
-- function.
functionSignature :: Function -> (Builder, Env)
functionSignature fun =
  L.fold goArgId (funArgs fun)
    & maybe id addClosure (funClosure fun)
    & first (functionHeader retType (funName fun))
  where
    retType
      | funInternal fun = "static " <> ctype
      | otherwise = ctype

    addClosure clsr (args0, bindings0) =
      let name = "ldst_closure" -- This name is always free
          bindings = Map.fromList
            $ zip clsr
            $ fmap (\idx -> StackVar $ name <> idx)
            $ brackets . B.intDec <$> [0..]
       in (ctype <> "* " <> name : args0, bindings0 <> bindings)

    goArgId = (,)
      <$> (\idn -> ctype <> B.char7 ' ' <> identForC idn) `L.premap` L.list
      <*> (\idn -> (idn, StackVar (identForC idn))) `L.premap` L.map

-- | @functionDeclDef signature body@ returns a pair of @(declaration, definition)@.
--
-- The @signature@ should be built by 'functionSignature'.
functionDeclDef :: Builder -> CStmt -> (Builder, Builder)
functionDeclDef signature (CStmt body) =
  let function = mconcat
        [ signature
        , "\n{\n"
        , body
        , "}\n\n"
        ]
   in (signature <> ";\n", function)

generateFunction :: Function -> Either String (Builder, Builder)
generateFunction topLevelFun = evalQueueT [topLevelFun] $ execWriterT go
  where
    go = lift popQ >>= \case
      Nothing -> pure ()
      Just fun -> lift (generateFunction' fun) >>= tell >> go

generateFunction' :: Function -> QueueT Function (Either String) (Builder, Builder)
generateFunction' fun = do
  let (sig, bindings) = functionSignature fun

  let genBody = do
        result <- generateExp (funBody fun)
        tellStmt $ terminate $ "return " <> unCExp result

  let info = Info
        { _infoBindings = bindings
        , _infoNameHint = funHint fun
        , _infoFuncHint = funName fun
        , _infoIndent = 1
        }

  evalRWST genBody info 0
    & fmap snd -- We only care about the writer result
    & fmap (functionDeclDef sig)

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
  NatRec{} -> throwError "natrec: not yet implemented"
  Var name -> do
    -- If there is a bound variable, return its value. Otherwise we assume
    -- there is a top level symbol with the matching name which we call to
    -- obtain its value.
    let topLevelCall = CExp $ callExp (functionForC name) []
    maybe topLevelCall varToExp <$> view (infoBindings . at name)
  Unit -> do
    pure $ CExp $ parens ctype <> unitInit
  Lab lbl -> do
    mkValue TagLabel lbl
  e@(Lam _ argId _ body) -> do
    name <- fresh (Just "lam")
    closure <- mkClosure $ fv e
    lift $ pushQ $ Function
      { funName = name
      , funHint = "lam"
      , funArgs = Just argId
      , funBody = body
      , funClosure = Just $! closureVars closure
      , funInternal = True
      }
    mkValue TagLam (name, closure)
  Rec{} -> throwError "rec: not yet implemented"
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
  Fork _ -> throwError "fork: not yet implemented"
  New _ -> throwError "new: not yet implemented"
  Send _ -> throwError "send: not yet implemented"
  Recv _ -> throwError "recv: not yet implemented"
  Case e cs -> do
    -- TODO: It is possible to arrange the comparisons to find the correct
    -- branch in O(log n) steps.
    --
    -- TODO: Should we assume that the matching branch always exists? Or check
    -- all branches and panic, in case none matches?
    label <- access TagLabel <$> (stmt =<< generateExp e)
    result <- declareFresh @'Stack ctype unitInit
    let buildBranch :: (String, Exp) -> StateT Builder GenM ()
        buildBranch (branchLabel, branchExp) = do
          ifB <- get <* put "else if "
          let cmpExp = callExp funStrcmp [label, labelForC branchLabel] <> " == 0"
          lift $ tellStmt $ CStmt $ callExp ifB [cmpExp] <> " {"
          lift $ local (infoIndent +~ 1) do
            e' <- generateExp branchExp
            tellStmt $ terminate $ bunwords [ unCExp (varToExp result), B.char7 '=', unCExp e' ]
          lift $ tellStmt $ CStmt "}"
    evalStateT (traverse_ buildBranch cs) ("if " :: Builder)
    pure $ varToExp result

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

functionHeader :: Builder -> Builder -> [Builder] -> Builder
functionHeader ret name args =
  ret <> B.char7 ' ' <> callExp name args

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

declareFresh :: forall x. Known x => Builder -> Builder -> GenM (CVar x)
declareFresh varType initExp = do
  name <- fresh Nothing
  tellStmt $ terminate $ mconcat
    [ varType
    , case sing @_ @x of
        SStack -> " "
        SHeap -> " *"
    , name
    , " = "
    , initExp
    ]
  pure case sing @_ @x of
         SStack -> StackVar name
         SHeap -> HeapVar name

unitInit :: Builder
unitInit = "{ 0 }"

-- | Writes the result of the given expression into a fresh variable.
stmt :: CExp -> GenM (CVar 'Stack)
stmt = declareFresh ctype . unCExp

-- | Clones the result of the given expression into a fresh variable which
-- lives on the heap instead of the stack.
clone :: CExp -> GenM (CVar 'Heap)
clone e = do
  var <- declareFresh ctype $ callExp funMalloc [sizeofCtype]
  tellAssign var e
  pure var

-- | Glues the parts together to yield something looking like a function call.
-- It is also used to generate function headers and control structures.
callExp :: Builder -> [Builder] -> Builder
callExp f args = f <> parens (intercalate ", " args)

tellAssign :: CVar x -> CExp -> GenM ()
tellAssign var val = tellStmt $ terminate $ bunwords
  [ unCExp (varToExp var)
  , B.char7 '='
  , unCExp val
  ]

tellAssignI :: CVar 'Heap -> Int -> CExp -> GenM ()
tellAssignI var idx val = tellStmt $ terminate $ bunwords
  [ varName var <> brackets (B.intDec idx)
  , B.char7 '='
  , unCExp val
  ]

funMalloc :: Builder
funMalloc = "malloc"

funStrcmp :: Builder
funStrcmp = "strcmp"

sizeofCtype :: Builder
sizeofCtype = callExp "sizeof" [ctype]

takeAddress :: CVar x -> Builder
takeAddress = \case
  StackVar v -> B.char7 '&' <> v
  HeapVar v -> v


-- | Adds some generated code to the output.
--
-- /Note:/ It is the callers job to include the trailing semicolon.
tellStmt :: CStmt -> GenM ()
tellStmt (CStmt s) = do
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

mkClosure :: Set Ident -> GenM Closure
mkClosure vars = do
  knownVars <- view infoBindings
  let (capturedVars, captureExprs) =
        unzip
          $ Map.toAscList
          $ Map.restrictKeys knownVars vars
  let capturedCount = length captureExprs
  expr <- if capturedCount == 0
    then do
      -- `malloc` of size zero is not allowed, use a NULL closure instead.
      pure (B.char7 '0')
    else do
      let size = B.intDec (length captureExprs) <> " * " <> sizeofCtype
      closure <- declareFresh @'Heap ctype $ callExp funMalloc [size]
      ifor_ captureExprs \idx expr ->
        tellAssignI closure idx (varToExp expr)
      pure $ varName closure
  pure Closure
    { closureVars = capturedVars
    , closureExpr = expr
    }

mkValue :: Tag a -> a -> GenM CExp
mkValue tag a = liftValue tag <$> case tag of
  TagInt -> pure $ B.intDec a
  TagLabel -> pure $ labelForC a
  TagPair -> do
    let (x, y) = a
    x' <- takeAddress <$> clone x
    y' <- takeAddress <$> clone y
    pure $ braceList Nothing [x', y']
  TagLam -> do
    let (fun, closure) = a
    pure $ braceList Nothing [fun, closureExpr closure]

liftValue :: Tag a -> Builder -> CExp
liftValue tag a = CExp
  $ braceList (Just ctype)
  $ pure
  $ bunwords
      [ B.char7 '.' <> tagAccessor tag
      , B.char7 '='
      , a
      ]

braceList :: Maybe Builder -> [Builder] -> Builder
braceList annot bs =
  foldMap parens annot <> braces (intercalate ", " bs)

access :: Tag a -> CVar x -> Builder
access tag v = unCExp (varToExp v) <> B.char7 '.' <> tagAccessor tag

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
  TagInt   -> "val_int"
  TagLabel -> "val_label"
  TagPair  -> "val_pair"
  TagLam   -> "val_lam"

tagType :: Tag a -> Builder -> Builder
tagType tag n = bunwords case tag of
  TagInt   -> ["int", n]
  TagLabel -> ["const char*", n]
  TagPair  -> [ctype <> B.char7 '*', n <> "[2]"]
  TagLam   -> [lambdaType, n]

-- | Concatenate a list of builders using a single space character.
bunwords :: [Builder] -> Builder
bunwords = intercalate (B.char7 ' ')

-- | Concatenate a list of builders using a single newline character.
--
-- This differs from 'unlines' which also appends a trailing newline.
bunlines :: [Builder] -> Builder
bunlines = intercalate (B.char7 '\n')

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

-- | Wraps the given builder in brackets.
--
-- @
-- brackets b === surround "[" "]" b
-- @
brackets :: Builder -> Builder
brackets = surround (B.char7 '[') (B.char7 ']')

-- | Appends a semicolon to the given builder
--
-- @
-- terminate b === b <> ";"
-- @
terminate :: Builder -> CStmt
terminate b = CStmt $ b <> B.char7 ';'

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
