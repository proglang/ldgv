{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
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
import Data.Semigroup as S
import Data.Set (Set)
import Data.String
import Data.Version
import Numeric
import Singletons
import Syntax
import Validation
import qualified Control.Foldl as L
import qualified Data.ByteString.Builder as B
import qualified Data.Char as C
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Paths_ldgv

-- | Represents values lifted into the @LDST_t@ type.
newtype CExp = CExp { unCExp :: Builder }
-- TODO: By using an ADT to differentiate what the expression might represent
-- we could generate more idiomatic code. This isn't terribly necessary though,
-- the common C compilers are able to understand and optimize our intentions
-- quite well.

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
  , funRecursive :: !(Maybe Ident)
    -- ^ @Just ident@ if this function can call itself recursively with
    -- identifier @ident@.
  }
-- TODO: In the current implementation, a function can have either both an
-- argument and a closure or neither. This should be made explicit in the type.

data Closure = Closure
  { closureVars :: ![Ident]
    -- ^ List of captured identifiers. The order corresponds to the order in
    -- the C code in 'closureExpr'.
  , closureExpr :: !Builder
    -- ^ An expression of type @union LDST_t*@.
  }

-- | A mapping from locally bound variables to their corresponding 'CVar'.
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
    (StackT Function (Either String))

newtype StackT s m a = StackT { unStackT :: StateT [s] m a }
  deriving newtype (Functor, Applicative, Monad, MonadError e, MonadWriter w)

evalStackT :: Monad m => [s] -> StackT s m a -> m a
evalStackT ss = flip evalStateT ss . unStackT

pushStack :: Monad m => s -> StackT s m ()
pushStack s = StackT $ modify (s :)

popStack :: Monad m => StackT s m (Maybe s)
popStack = StackT do
  ss <- get
  case ss of
    [] -> pure Nothing
    s:ss' -> Just s <$ put ss'

data GenMonoid = GenMonoid
  { genSigs :: !(Map Ident (Maybe (S.Last Type)))
  , genDecls :: !Builder
  , genDefs :: !Builder
  }

instance Semigroup GenMonoid where
  GenMonoid a1 b1 c1 <> GenMonoid a2 b2 c2 =
    GenMonoid (Map.unionWith (<>) a1 a2) (b1 <> b2) (c1 <> c2)

instance Monoid GenMonoid where
  mempty = GenMonoid mempty mempty mempty

generate :: Maybe Ident -> [Decl] -> Either String Builder
generate entryPoint = joinParts . first concatErrors . validationToEither . foldMap \case
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
          , funRecursive = Nothing
          }

    let addContext err =
          "in function ‘" ++ name ++ "’:\n" ++ err

    let identMap = Map.singleton name Nothing

    eitherToValidation
      $ bimap (pure . addContext) (uncurry $ GenMonoid identMap)
      $ generateFunction root

  DSig name _ typ ->
    pure $ mempty{ genSigs = Map.singleton name $ Just $ S.Last typ }

  _ ->
    -- Nothing to generate for this kind of top level thingy.
    mempty

  where
    joinParts errOrGM = errOrGM >>= \gm ->
      let gm' = case entryPoint of
                  Nothing -> pure gm
                  Just ep -> genMainFunction ep gm
       in glueCode <$> fmap genDecls gm' <*> fmap genDefs gm'

    concatErrors :: NonEmpty String -> String
    concatErrors = intercalate "\n\n" . toList

genMainFunction :: Ident -> GenMonoid -> Either String GenMonoid
genMainFunction mainId gm = case Map.lookup mainId (genSigs gm) of
  Nothing -> Left $
    "entry point: unknown identifier ‘" <> mainId <> "’"

  Just Nothing -> Left $
    "entry point: no type signature for identifier ‘" <> mainId <> "’"

  Just (Just (S.Last ty)) ->
    let (_, mainFunction) = functionDeclDef "int main(void)" $ foldMap stmtLine
          [ terminate $ ctype <> " result = " <> callExp (functionForC mainId) []
          , -- Prohibit "unused variable" warnings in the generated C code in
            -- case 'explainExpression' only outputs a static string.
            terminate "(void)result"
          , explainExpression ty (StackVar "result")
          ]
        stmtLine s = mconcat [ CStmt "  ", s, CStmt "\n" ]
     in Right $ gm { genDefs = genDefs gm <> mainFunction }

-- | Generates a call to @printf@ which tries to output the value of the given
-- variable according to the given type. In case the type has non-printable
-- values (e.g. a function type) only the type is printed.
explainExpression :: Type -> CVar 'Stack -> CStmt
explainExpression ty0 v0 =
  let format :: Type -> CVar x -> (Endo String, Endo [Builder])
      format ty v = case ty of
        TUnit -> literal "()"
        TInt -> formatted "Int %d" $ access TagInt v
        TNat -> formatted "Nat %d" $ access TagInt v
        TLab _ -> formatted "Label %s" $ access TagLabel v
        TPair _ _ t1 t2 ->
          let (v1, v2) = accessPair v in
          mconcat
            [ literal "<"
            , format t1 v1
            , literal ", "
            , format t2 v2
            , literal ">"
            ]
        _ -> (Endo $ showsPrec 11 ty, mempty)

      literal s =
        (Endo (showString s), mempty)
      formatted s val =
        (Endo (showString s), Endo (val :))

      (Endo fmt, Endo args) = format ty0 v0
      fmt' = (showString "result: " . fmt) "\n"
   in terminate $ callExp "printf" (escapedCString fmt' : args [])

-- | Builds a function signature, an 'Env' binding the arguments to the
-- function, including variables bound through the closure.
--
-- The function signature convention is
--
-- @
-- void /function-name/(
--    struct LDST_t *closure,
--    struct LDST_t argument)
-- @
--
-- where @closure@ and @argument@ are only present for non-toplevel bindings,
-- including the curried forms of toplevel bindings.
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
      let bindings = Map.fromList
            $ zip clsr
            $ fmap (\idx -> StackVar $ cClosureName <> idx)
            $ brackets . B.intDec <$> [0..]
       in (ctype <> "* " <> cClosureName : args0, bindings0 <> bindings)

    goArgId = (,)
      <$> (\idn -> ctype <> B.char7 ' ' <> identForC idn) `L.premap` L.list
      <*> (\idn -> (idn, StackVar (identForC idn))) `L.premap` L.map

-- | The parameter name given to the closure argument.
--
-- Mainly an implementation detail of 'functionSignature' but required to
-- generate recursive bindings for the @rec@ construct. See 'generateFunction''
-- for more information.
cClosureName :: Builder
cClosureName = "_ldst_closure"

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
generateFunction topLevelFun = evalStackT [topLevelFun] $ execWriterT go
  where
    go = lift popStack >>= \case
      Nothing -> pure ()
      Just fun -> lift (generateFunction' fun) >>= tell >> go

generateFunction' :: Function -> StackT Function (Either String) (Builder, Builder)
generateFunction' fun = do
  let (sig, bindings) = functionSignature fun

  let genBody = do
        -- Where do we add the binding for recursive functions? It requires
        -- access to the closure argument, which we don't really want outside
        -- of 'functionSignature'.
        --
        -- On the other hand we can't really do it inside there either because
        -- we want to declare a variable which holds the LDST_t value,
        -- requiring us to be inside 'GenM'.
        --
        -- For now we fixed the closure argument name globally giving us access
        -- here.
        insertRecArg <- case funRecursive fun of
          Nothing -> pure id
          Just recId -> do
            -- It is possible that the recusion name recId shadows the
            -- functions argument name, but this follows the typechecker rules!
            -- 
            -- If
            --
            --    val check = rec x (x : Int) : Int = x
            --
            -- typechecks 'recId' should *not* shadow an existing variable, if
            -- it fails to typecheck, it *should* shadow the variable.
            recVal <- mkValue TagLam (funName fun, Closure [] cClosureName)
            pure $ infoBindings . at recId ?~ recVal

        local insertRecArg do
          result <- generateExp (funBody fun)
          tellStmt $ terminate $ "return " <> unCExp (varToExp result)

  let info = Info
        { _infoBindings = bindings
        , _infoNameHint = funHint fun
        , _infoFuncHint = funName fun
        , _infoIndent = 1
        }

  evalRWST genBody info 0
    & fmap snd -- We only care about the WriterT result.
    & fmap (functionDeclDef sig)

generateExp :: Exp -> GenM (CVar 'Stack)
generateExp = \case
  Let v a b -> do
    scoped v (generateExp a) (const $ generateExp b)
  Lit l -> generateLiteral l
  Math m -> generateMath m
  Succ e -> do
    e' <- generateExp e
    liftValue TagInt $ access TagInt e' <> " + 1"
  NatRec{} -> throwError "natrec: not yet implemented"
  Var name -> do
    -- If there is a bound variable, return its value. Otherwise we assume
    -- there is a top level symbol with the matching name which we call to
    -- obtain its value.
    let topLevelCall = CExp $ callExp (functionForC name) []
    storeVar . maybe topLevelCall varToExp =<< view (infoBindings . at name)
  e@(Lam _ argId _ body) -> do
    name <- fresh (Just "lam")
    closure <- mkClosure $ fv e
    lift $ pushStack $ Function
      { funName = name
      , funHint = "lam"
      , funArgs = Just argId
      , funBody = body
      , funClosure = Just $! closureVars closure
      , funInternal = True
      , funRecursive = Nothing
      }
    mkValue TagLam (name, closure)
  e@(Rec recId argId _ _ body) -> do
    name <- fresh (Just "rec")
    closure <- mkClosure $ fv e
    lift $ pushStack $ Function
      { funName = name
      , funHint = "rec"
      , funArgs = Just argId
      , funBody = body
      , funClosure = Just $! closureVars closure
      , funInternal = True
      , funRecursive = Just recId
      }
    mkValue TagLam (name, closure)
  App funExp argExp -> do
    -- TODO: Directly call statically known top level functions.
    lam <- generateExp funExp
    arg <- generateExp argExp
    let (fun, closure) = accessLambda lam
    storeVar $ CExp $ callExp fun [closure, unCExp $ varToExp arg]
  Pair _ idnA a b -> do
    scoped idnA (generateExp a) \a' -> do
      b' <- generateExp b
      mkValue TagPair (varToExp a', varToExp b')
  LetPair idnFst idnSnd pairExp body -> do
    -- Evaluate pairExp first.
    scoped (idnFst ++ "_" ++ idnSnd) (generateExp pairExp) \pairVar ->
      let (valFst, valSnd) = accessPair pairVar
       in scoped idnFst (pure valFst) \_ ->
          scoped idnSnd (pure valSnd) \_ ->
            generateExp body
  Fst e -> fst . accessPair <$> generateExp e
  Snd e -> snd . accessPair <$> generateExp e
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
    label <- access TagLabel <$> generateExp e
    result <- newUnitVar
    let buildBranch :: (String, Exp) -> StateT Builder GenM ()
        buildBranch (branchLabel, branchExp) = do
          ifB <- get <* put "else if "
          let cmpExp = callExp funStrcmp [label, labelForC branchLabel] <> " == 0"
          lift $ tellStmt $ CStmt $ callExp ifB [cmpExp] <> " {"
          lift $ local (infoIndent +~ 1) do
            e' <- generateExp branchExp
            tellStmt $ terminate $ bunwords
              [ unCExp (varToExp result)
              , B.char7 '='
              , unCExp (varToExp e')
              ]
          lift $ tellStmt $ CStmt "}"
    evalStateT (traverse_ buildBranch cs) ("if " :: Builder)
    pure result

generateMath :: MathOp Exp -> GenM (CVar 'Stack)
generateMath = liftValue TagInt <=< \case
  Add a b -> math '+' a b
  Sub a b -> math '-' a b
  Mul a b -> math '*' a b
  Div a b -> math '/' a b
  Neg a   -> do
    a' <- generateExp a
    pure $ B.char7 '-' <> access TagInt a'
  where
    math c a b = do
      a' <- generateExp a
      b' <- generateExp b
      pure $ bunwords [ access TagInt a', B.char7 c, access TagInt b' ]

generateLiteral :: Literal -> GenM (CVar 'Stack)
generateLiteral = \case
  LInt i -> mkValue TagInt i
  LNat n -> mkValue TagInt n
  LLab l -> mkValue TagLabel l
  LUnit  -> newUnitVar

scoped :: Ident -> GenM (CVar 'Stack) -> (CVar 'Stack -> GenM b) -> GenM b
scoped idn val body = do
  var <- local (infoNameHint .~ identForC idn) val
  local (infoBindings . at idn ?~ var) $ body var

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

newUnitVar :: GenM (CVar 'Stack)
newUnitVar = storeVar (CExp "{ 0 }")

-- | Writes the result of the given expression into a fresh variable.
storeVar :: CExp -> GenM (CVar 'Stack)
storeVar = declareFresh ctype . unCExp

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

mkValue :: Tag a -> a -> GenM (CVar 'Stack)
mkValue tag a = liftValue tag =<< case tag of
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

liftValue :: Tag a -> Builder -> GenM (CVar 'Stack)
liftValue tag a = storeVar
  $ CExp
  $ braceList Nothing
  $ pure
  $ bunwords
      [ B.char7 '.' <> tagAccessor tag
      , B.char7 '='
      , a
      ]

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

header :: Builder
header = bunlines
  [ "//"
  , "// Generated by ldgv v" <> fromString (showVersion Paths_ldgv.version)
  , "//"
  , ""
  , "#include <stdio.h>"
  , "#include <stdlib.h>    // malloc"
  , "#include <string.h>    // strcmp"
  , ""
  , "// Forward declarations"
  , unCStmt $ terminate ctype
  , unCStmt $ terminate cLambdaType
  , ""
    -- The definition for the lambda type has to come first, because it is a
    -- non-pointer field in the main type.
  , "// Type declarations"
  , cFunPtrDecl
  , cLambdaTypeDecl
  , ctypeDecl
  ]

-- | Concatenates a builder containing the function signatures and a builder
-- containing the function definitions with the 'header' containing the type
-- definitions.
glueCode :: Builder -> Builder -> Builder
glueCode decls defs = bunlines
  [ header
  , ""
  , "// Generated code - forward declarations"
  , decls
  , "// Generated code - function definitions"
  , defs
  ]

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

ctypeDecl :: Builder
ctypeDecl = compoundDefinition ctype
  [ tag TagInt
  , tag TagLabel
  , tag TagLam
  , tag TagPair
  ]
  where
    tag t = tagType t (tagAccessor t)

-- | The type of lambdas and closures in the generated C code.
--
-- @
-- struct LDST_lam_t {
--   LDST_fp fp;
--   LDST_t *closure;
-- };
-- @
cLambdaType :: Builder
cLambdaType = "struct LDST_lam_t"

cLambdaTypeDecl :: Builder
cLambdaTypeDecl = compoundDefinition cLambdaType
  [ cFunPtr <+> "lam_fp"
  , ctype <+> "*lam_closure"
  ]

-- | The type of pointers to non-toplevel functions.
--
-- @
-- typedef void (*LDST_fp)(struct LDST_cont_t*, union LDST_t*, union LDST_t);
-- @
--
-- If this changes the signature generation in 'functionSignature' has to
-- be adjusted as well.
cFunPtr :: Builder
cFunPtr = "LDST_fp_t"

cFunPtrDecl :: Builder
cFunPtrDecl = mconcat
  [ "typedef "
  , functionHeader "void" (parens $ pointer <> cFunPtr)
      [ ctype <> pointer
      , ctype
      ]
  , B.char7 ';'
  ]
  where
    pointer = B.char7 '*'

-- | Generates a definition of a compound type, that is a struct or union type.
--
-- The first argument is the name including either @"struct "@ or @"union @" at
-- the beginning, the field declarations should not be terminated with a
-- semicolon.
compoundDefinition :: Builder -> [Builder] -> Builder
compoundDefinition name fields =
  intercalate "\n  " (name <> " {" : fmap (<> B.char7 ';') fields) <> "\n};"

braceList :: Maybe Builder -> [Builder] -> Builder
braceList annot bs =
  foldMap parens annot <> braces (intercalate ", " bs)

access :: Tag a -> CVar x -> Builder
access tag v = unCExp (varToExp v) <> B.char7 '.' <> tagAccessor tag

accessPair :: CVar x -> (CVar 'Stack, CVar 'Stack)
accessPair v =
  let b = access TagPair v
   in (StackVar $ b <> "[0]", StackVar $ b <> "[1]")

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
  TagLam   -> [cLambdaType, n]

-- | Concatenates two builders using a single space character.
(<+>) :: Builder -> Builder -> Builder
a <+> b = a <> B.char7 ' ' <> b
infixr 6 <+>

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

-- | Turn an identifier into a function name suitable in the generated C code.
-- It uses the encoding from 'identForC' and prepends @"ldst__"@
functionForC :: Ident -> Builder
functionForC idn = "ldst__" <> identForC idn

labelForC :: String -> Builder
labelForC = escapedCString

-- | Escapes a string value as a string literal in C, including the surrounding
-- quotes.
--
-- If the string contains non-ASCII characters the resulting C code requires
-- compilation with C11 as the @\\Unnnnnnnn@ escape sequence is used.
escapedCString :: String -> Builder
escapedCString = surround (B.char7 '"') (B.char7 '"') . B.string7 . concatMap \c ->
  let hex = showHex (C.ord c) ""
      hexPadded n = replicate (n - length hex) '0' ++ hex
  in
  if | c == '"' -> ['\\', '"']
     | c == '\\' -> ['\\', '\\']
     | C.isAscii c && C.isPrint c -> [c]
     | C.isAscii c -> '\\':'x':hexPadded 2
     | otherwise -> '\\':'U':hexPadded 8
