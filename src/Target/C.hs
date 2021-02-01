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
import Data.Coerce
import Data.Foldable
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Proxy
import Data.Semigroup as S
import Data.Set (Set)
import Data.String
import Data.Version
import Numeric
import Syntax.CPS
import Validation
import qualified Data.ByteString.Builder as B
import qualified Data.Char as C
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Paths_ldgv
import qualified Syntax as S

-- | Type level tag for values.
data V

-- | Type level tag for lambdas.
data L

-- | Type level tag for continuations.
data K

-- | Type level tag for a pointer to @a@.
data Pointer a

-- | Represents values lifted into the type correspoding to @t@.
newtype CExp t = CExp { unCExp :: Builder }
-- TODO: By using an ADT to differentiate what the expression might represent
-- we could generate more idiomatic code. This isn't terribly necessary though,
-- the common C compilers are able to understand and optimize our intentions
-- quite well.

newtype CVar t = CVar Builder

newtype CStmt = CStmt { unCStmt :: Builder }
  deriving newtype (Semigroup, Monoid)

data Tag a where
  TagInt :: Tag Int
  TagLabel :: Tag String
  TagPair :: Tag (CExp V, CExp V)
  TagLam :: Tag (CExp L)

data FunctionHeader = FunctionHeader
  { funName :: !Builder
  , funArgs :: !(Maybe ([Ident], Ident))
    -- ^ A pair of the identifiers carried by the closure parameter and the
    -- functions argument.
  , funInternal :: !Bool
    -- ^ @True@ if this function is only used internally and should get
    -- @static@ linkage.
    --
    -- Internal functions originate from lambda expressions while the nullary
    -- top level functions correspond to non-internal functions.
  }

data Function = Function
  { funHeader  :: !FunctionHeader
  , funHint    :: !Builder
    -- ^ See '_infoNameHint'.
  , funBody    :: !Exp
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
  , closureExpr :: !(CExp (Pointer V))
    -- ^ An expression of type @union LDST_t*@.
  }

-- | A mapping from locally bound variables to their corresponding 'CVar'.
type Env = Map Ident (CVar V)

data Info = Info
  { _infoBindings :: !Env
    -- ^ Mapping from bound variables to the corresponding identifiers in the
    -- generated C code.

  , _infoContinuation :: !(CVar (Pointer K))
    -- ^ The current continuation.

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

class ExpLike e where
  toCExp :: e t -> CExp t

instance ExpLike CExp where
  toCExp = id

instance ExpLike CVar where
  toCExp = coerce

class CType t where
  typeName :: proxy t -> Builder

instance CType V where
  typeName _ = ctype

instance CType L where
  typeName _ = cLambdaType

instance CType K where
  typeName _ = cContType

instance CType a => CType (Pointer a) where
  typeName _ = typeName @a Proxy <> B.char7 '*'

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

generate :: Maybe Ident -> [S.Decl] -> Either String Builder
generate entryPoint = joinParts . first concatErrors . validationToEither . foldMap \case
  S.DFun name args body _ -> do
    -- Curry the function.
    let lambdaBody = foldr (\(m, idn, ty) -> S.Lam m idn ty) body args

    let root = Function
          { funHeader = topLevelHeader name
          , funHint = identForC name
          , funBody = toCPS lambdaBody
          , funRecursive = Nothing
          }

    let addContext err =
          "in function ‘" ++ name ++ "’:\n" ++ err

    let identMap = Map.singleton name Nothing

    eitherToValidation
      $ bimap (pure . addContext) (uncurry $ GenMonoid identMap)
      $ generateFunction root

  S.DSig name _ typ -> do
    -- When we encounter a signature we also have to emit this functions
    -- top-level reference declaration, otherwise it will be missing when the
    -- function is used but no definition is given.
    let (sig, _, _) = functionSignature $ topLevelHeader name
    let gen = mempty
          { genSigs = Map.singleton name $ Just $ S.Last typ
          , genDecls = sig <> ";\n"
          }
    pure gen

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

topLevelHeader :: Ident -> FunctionHeader
topLevelHeader funIdent = FunctionHeader
  { funName = functionForC funIdent
  , funArgs = Nothing
  , funInternal = False
  }

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
          , explainExpression ty (CVar "result")
          ]
        stmtLine s = mconcat [ CStmt "  ", s, CStmt "\n" ]
     in Right $ gm { genDefs = genDefs gm <> mainFunction }

-- | Generates a call to @printf@ which tries to output the value of the given
-- variable according to the given type. In case the type has non-printable
-- values (e.g. a function type) only the type is printed.
explainExpression :: Type -> CVar V -> CStmt
explainExpression ty0 v0 =
  let format :: Type -> CVar V -> (Endo String, Endo [Builder])
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
-- function (including variables bound through the closure), and the variable
-- containing the continuation.
--
-- The function signature convention is
--
-- @
-- void /function-name/(
--    struct LDST_cont_t *continuation,
--    struct LDST_t *closure,
--    struct LDST_t argument)
-- @
--
-- where @closure@ and @argument@ are only present for non-toplevel bindings,
-- including the curried forms of toplevel bindings. If this changes
-- 'cFunPtrDecl' has to be adjusted as well.
functionSignature :: FunctionHeader -> (Builder, Env, CVar (Pointer K))
functionSignature fun =
  foldMap listArgs (funArgs fun)
    & addContinuation
    & first (functionHeader retType (funName fun))
    & \(sig, bindings) -> (sig, bindings, CVar cContName)
  where
    retType
      | funInternal fun = "static void"
      | otherwise = "void"

    addContinuation =
      _1 %~ (cContType <> "* " <> cContName :)

    listArgs (closure, argId) =
      ( [ typeName @(Pointer V) Proxy <+> cClosureName
        , typeName @V Proxy <+> identForC argId
        ]
      , Map.insert argId (CVar (identForC argId))
          $ Map.fromList
          $ zip closure
          $ fmap (\idx -> CVar $ cClosureName <> idx)
          $ brackets . B.intDec <$> [0..]
      )

    cContName = "_ldst_k"

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
  let (sig, bindings, initialK) = functionSignature (funHeader fun)
  let name = funName $ funHeader fun

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
            recVal <- mkValue TagLam . toCExp =<< mkLambda' name (CExp cClosureName)
            pure $ infoBindings . at recId ?~ recVal

        local insertRecArg $
          generateExp (funBody fun)

  let info = Info
        { _infoBindings = bindings
        , _infoContinuation = initialK
        , _infoNameHint = funHint fun
        , _infoFuncHint = name
        , _infoIndent = 1
        }

  evalRWST genBody info 0
    & fmap snd -- We only care about the WriterT result.
    & fmap (functionDeclDef sig)

generateVal :: Val -> GenM (CVar V)
generateVal = \case
  Lit l -> generateLiteral l
  Var name ->
    -- The unsafe operator (^?!) is "safe" here because if the variable is not
    -- locally bound the CPS transformation should have generated a 'TLCall'
    -- node.
    asks \env -> env ^?! infoBindings . ix name
  e@(Lam _ argId _ body) -> do
    name <- fresh (Just 'l')
    closure <- mkClosure $ fv e
    hint <- view infoNameHint
    lift $ pushStack $ Function
      { funHeader = FunctionHeader
          { funName = name
          , funArgs = Just (closureVars closure, argId)
          , funInternal = True
          }
      , funHint = hint
      , funBody = body
      , funRecursive = Nothing
      }
    mkValue TagLam =<< mkLambda name closure
  e@(Rec recId argId _ _ body) -> do
    name <- fresh (Just 'r')
    closure <- mkClosure $ fv e
    hint <- view infoNameHint
    lift $ pushStack $ Function
      { funHeader = FunctionHeader
          { funName = name
          , funArgs = Just (closureVars closure, argId)
          , funInternal = True
          }
      , funHint = hint
      , funBody = body
      , funRecursive = Just recId
      }
    mkValue TagLam =<< mkLambda name closure
  Math m -> generateMath m
  Succ e -> do
    e' <- generateVal e
    liftValue TagInt $ access TagInt e' <> " + 1"
  Pair a b -> do
    a' <- generateVal a
    b' <- generateVal b
    mkValue TagPair (toCExp a', toCExp b')
  Fork _ -> throwError "fork: not yet implemented"
  New _ -> throwError "new: not yet implemented"
  Send _ -> throwError "send: not yet implemented"

generateExp :: Exp -> GenM ()
generateExp = \case
  Return val -> generateVal val >>= invokeContinuation
  Let v a b -> do
    scoped v (generateVal a) (const $ generateExp b)
  LetPair idnFst idnSnd pairExp body -> do
    scoped "letpair" (generateVal pairExp) \pairVar ->
      let (valFst, valSnd) = accessPair pairVar
       in scoped idnFst (pure valFst) \_ ->
          scoped idnSnd (pure valSnd) \_ ->
            generateExp body
  LetCont k e -> do
    k' <- generateContinuationM $ Just k
    local (infoContinuation .~ k') $ generateExp e
  Call funExp argExp mk -> do
    lam <- generateVal funExp
    arg <- generateVal argExp
    k <- generateContinuationM mk
    invoke (accessValLambda lam) k arg
  TLCall funId mk -> do
    k <- generateContinuationM mk
    invoke' (functionForC funId) k []
  Case e cs -> do
    -- TODO: It is possible to arrange the comparisons to find the correct
    -- branch in O(log n) steps.
    --
    -- TODO: Should we assume that the matching branch always exists? Or check
    -- all branches and panic, in case none matches?
    label <- access TagLabel <$> generateVal e
    let buildBranch :: (String, Exp) -> StateT Builder GenM ()
        buildBranch (branchLabel, branchExp) = do
          ifB <- get <* put "else if "
          let cmpExp = callExp funStrcmp [label, labelForC branchLabel] <> " == 0"
          lift $ tellStmt $ CStmt $ callExp ifB [cmpExp] <> " {"
          lift $ local (infoIndent +~ 1) $ generateExp branchExp
          lift $ tellStmt $ CStmt "}"
    evalStateT (traverse_ buildBranch cs) ("if " :: Builder)
  NatRec{} -> throwError "natrec: not yet implemented"
  Recv _ _ -> throwError "recv: not yet implemented"

generateMath :: MathOp Val -> GenM (CVar V)
generateMath = liftValue TagInt <=< \case
  Add a b -> math '+' a b
  Sub a b -> math '-' a b
  Mul a b -> math '*' a b
  Div a b -> math '/' a b
  Neg a   -> do
    a' <- generateVal a
    pure $ B.char7 '-' <> access TagInt a'
  where
    math c a b = do
      a' <- generateVal a
      b' <- generateVal b
      pure $ bunwords [ access TagInt a', B.char7 c, access TagInt b' ]

generateLiteral :: Literal -> GenM (CVar V)
generateLiteral = \case
  LInt i -> mkValue TagInt i
  LNat n -> mkValue TagInt n
  LLab l -> mkValue TagLabel l
  LUnit  -> newUnitVar

generateContinuationM :: Maybe Continuation -> GenM (CVar (Pointer K))
generateContinuationM Nothing = view infoContinuation
generateContinuationM (Just (resId, kbody)) = do
  kname <- fresh (Just 'k')
  closure <- mkClosure (fv kbody)
  hint <- view infoNameHint
  lift $ pushStack Function
    { funHeader = FunctionHeader
        { funName = kname
        , funArgs = Just (closureVars closure, resId)
        , funInternal = True
        }
    , funHint = hint
    , funBody = kbody
    , funRecursive = Nothing
    }
  klam <- mkLambda kname closure
  kvar <- mkContinuation klam =<< view infoContinuation
  clone kvar

invokeContinuation :: ExpLike e => e V -> GenM ()
invokeContinuation e = do
  k <- view infoContinuation
  let (klam, knext) = accessContinuation $ accessPointer k
  -- TODO: free the used continuation.
  invoke klam knext e

invoke :: (ExpLike e1, ExpLike e2) => CVar L -> e1 (Pointer K) -> e2 V -> GenM ()
invoke lam k val = do
  let (fun, closure) = accessLambda lam
  invoke' fun k [unCExp closure, unCExp (toCExp val)]

invoke' :: ExpLike e => Builder -> e (Pointer K) -> [Builder] -> GenM ()
invoke' fun k args = tellStmt $ terminate $ callExp fun $ unCExp (toCExp k) : args

scoped :: Ident -> GenM (CVar V) -> (CVar V -> GenM b) -> GenM b
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
fresh :: Maybe Char -> GenM Builder
fresh funKind = do
  n <- get <* modify' (+1)
  hint <- case funKind of
    Nothing -> (\h -> h <> B.char7 '_') <$> view infoNameHint
    Just c  -> (\h -> h <> B.char7 '_' <> B.char7 c) <$> view infoFuncHint
  pure $ hint <> B.wordHex n

declareFresh :: forall t. CType t => Builder -> GenM (CVar t)
declareFresh initExp = do
  name <- fresh Nothing
  tellStmt $ terminate $ bunwords
    [ typeName @t Proxy
    , name
    , B.char7 '='
    , initExp
    ]
  pure $ CVar name

newUnitVar :: GenM (CVar V)
newUnitVar = storeVar (CExp "{ 0 }")

-- | Writes the result of the given expression into a fresh variable.
storeVar :: (CType t, ExpLike e) => e t -> GenM (CVar t)
storeVar = declareFresh . unCExp . toCExp

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
      pure nullPointer
    else do
      let size = B.intDec (length captureExprs) <> " * " <> cSizeof @V Proxy
      closure <- declareFresh $ callExp funMalloc [size]
      itraverse_ (tellAssignI closure) captureExprs
      pure $ toCExp closure
  pure Closure
    { closureVars = capturedVars
    , closureExpr = expr
    }

mkLambda :: Builder -> Closure -> GenM (CExp L)
mkLambda fun = fmap toCExp . mkLambda' fun . closureExpr

mkLambda' :: ExpLike e => Builder -> e (Pointer V) -> GenM (CVar L)
mkLambda' fun closure = declareFresh $ braceList [fun, unCExp $ toCExp closure]

accessLambda :: CVar L -> (Builder, CExp Closure)
accessLambda v = (accessRaw v "lam_fp", CExp $ accessRaw v "lam_closure")

mkContinuation :: (ExpLike e1, ExpLike e2) => e1 L -> e2 (Pointer K) -> GenM (CVar K)
mkContinuation lambda next = declareFresh $ braceList [unCExp $ toCExp lambda, unCExp $ toCExp next]

accessContinuation :: CVar K -> (CVar L, CVar (Pointer K))
accessContinuation v = (CVar (accessRaw v "k_lam"), CVar (accessRaw v "k_next"))

accessPointer :: CVar (Pointer t) -> CVar t
accessPointer (CVar v) = CVar $ parens $ B.char7 '*' <> v

mkValue :: Tag a -> a -> GenM (CVar V)
mkValue tag a = liftValue tag =<< case tag of
  TagInt -> pure $ B.intDec a
  TagLabel -> pure $ labelForC a
  TagPair -> do
    let (x, y) = a
    x' <- unCExp . toCExp <$> clone x
    y' <- unCExp . toCExp <$> clone y
    pure $ braceList [x', y']
  TagLam -> pure $ unCExp a

liftValue :: Tag a -> Builder -> GenM (CVar V)
liftValue tag a = storeVar
  $ CExp
  $ braceList
  $ pure
  $ bunwords
      [ B.char7 '.' <> tagAccessor tag
      , B.char7 '='
      , a
      ]

-- | Clones the result of the given expression into a fresh variable which
-- lives on the heap instead of the stack.
clone :: forall t e. (CType t, ExpLike e) => e t -> GenM (CVar (Pointer t))
clone e = do
  var <- declareFresh $ callExp funMalloc [cSizeof @t Proxy]
  tellAssignI var 0 e
  pure var

nullPointer :: CExp (Pointer t)
nullPointer = CExp $ B.char7 '0'

-- | Glues the parts together to yield something looking like a function call.
-- It is also used to generate function headers and control structures.
callExp :: Builder -> [Builder] -> Builder
callExp f args = f <> parens (intercalate ", " args)

tellAssignI :: ExpLike e => CVar (Pointer t) -> Int -> e t -> GenM ()
tellAssignI (CVar var) idx val = tellStmt $ terminate $ bunwords
  [ var <> brackets (B.intDec idx)
  , B.char7 '='
  , unCExp $ toCExp val
  ]

funMalloc :: Builder
funMalloc = "malloc"

funStrcmp :: Builder
funStrcmp = "strcmp"

cSizeof :: CType t => proxy t -> Builder
cSizeof = callExp "sizeof" . pure . typeName

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
  , unCStmt $ terminate cContType
  , ""
    -- The definition for the lambda type has to come first, because both the
    -- continuation type and the main LDST_t union contain non-pointer fields.
  , "// Type declarations"
  , cFunPtrDecl
  , cLambdaTypeDecl
  , cContTypeDecl
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

-- | The type of continuations in the generated C code.
--
-- @
-- struct LDST_cont_t {
--    LDST_fp fp;
--    union LDST_t *closure;
--    union LDST_cont_t *cont;
-- };
-- @
cContType :: Builder
cContType = "struct LDST_cont_t"

cContTypeDecl :: Builder
cContTypeDecl = compoundDefinition cContType
  [ cLambdaType <+> "k_lam"
  , cContType <+> "*k_next"
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
      [ cContType <> pointer
      , ctype <> pointer
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

braceList :: [Builder] -> Builder
braceList bs = braces (intercalate ", " bs)

accessRaw :: CVar t -> Builder -> Builder
accessRaw (CVar v) x = v <> B.char7 '.' <> x

access :: Tag a -> CVar V -> Builder
access tag v = accessRaw v (tagAccessor tag)

accessPair :: CVar V -> (CVar V, CVar V)
accessPair v =
  let b = access TagPair v
   in (CVar $ b <> "[0]", CVar $ b <> "[1]")

accessValLambda :: CVar V -> CVar L
accessValLambda = CVar . access TagLam

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
     | c == '\n' -> ['\\', 'n'] -- Not strictly necessary.
     | C.isAscii c && C.isPrint c -> [c]
     | C.isAscii c -> '\\':'x':hexPadded 2
     | otherwise -> '\\':'U':hexPadded 8
