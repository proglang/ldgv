{-# LANGUAGE ViewPatterns #-}
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
--
-- @
-- union LDST_t {
--   int val_int;
--   const char *val_label;
--   union LDST_t *val_pair;
--   struct LDST_lam_t val_lam;
-- };
-- @
data V

-- | Type level tag for lambdas.
--
-- @
-- struct LDST_lam_t {
--   LDST_fp fp;
--   LDST_t *closure;
-- };
-- @
data L

-- | Type level tag for channels.
data C

-- | Type level tag for continuations.
--
-- @
-- struct LDST_cont_t {
--    LDST_fp fp;
--    union LDST_t *closure;
--    union LDST_cont_t *cont;
-- };
-- @
data K

-- | Type level tag for @enum LDST_res_t@.
data R

-- | Type level tag for a pointer to @a@.
data Pointer a

-- | Represents an expression of type @t@.
newtype CExp t = CExp { unCExp :: Builder }
-- TODO: By using an ADT to differentiate what the expression might represent
-- we could generate more idiomatic code. This isn't terribly necessary though,
-- the common C compilers are able to understand and optimize our intentions
-- quite well.

-- | Represents a variable reference of type @t@.
newtype CVar t = CVar { unCVar :: Builder }

newtype CStmt = CStmt Builder
  deriving newtype (Semigroup, Monoid)

data Tag a where
  TagInt :: Tag Int
  TagLabel :: Tag String
  TagPair :: Tag (CExp V, CExp V)
  TagLam :: Tag (CExp L)
  TagChan :: Tag (CExp (Pointer C))

data FunctionArgs = FunctionArgs
  { funClosure  :: [Ident]
  , funArgIdent :: Ident
  , funRecIdent :: Maybe Ident
    -- ^ @Just ident@ if this function can call itself recursively with
    -- identifier @ident@.
  }

data FunctionHeader = FunctionHeader
  { funName :: !Builder
  , funArgs :: !(Maybe FunctionArgs)
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
  }

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
  typeName _ = "union LDST_t"
instance CType L where
  typeName _ = "struct LDST_lam_t"
instance CType C where
  typeName _ = "struct LDST_chan_t"
instance CType K where
  typeName _ = "struct LDST_cont_t"
instance CType R where
  typeName _ = cResType
instance CType () where
  typeName _ = "void"
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

  Just (Just (S.Last _ty)) ->
    Left "entry point generation is currently broken"
  {-
    let (_, mainFunction) = functionDeclDef "int main(void)" $ foldMap stmtLine
          [ terminate $ ctype <> " result = " <> callExp (functionForC mainId) []
          , -- Prohibit "unused variable" warnings in the generated C code in
            -- case 'explainExpression' only outputs a static string.
            terminate "(void)result"
          , explainExpression ty (CVar "result")
          ]
        stmtLine s = mconcat [ CStmt "  ", s, CStmt "\n" ]
     in Right $ gm { genDefs = genDefs gm <> mainFunction }
  -}

-- | Generates a call to @printf@ which tries to output the value of the given
-- variable according to the given type. In case the type has non-printable
-- values (e.g. a function type) only the type is printed.
_explainExpression :: Type -> CVar V -> CStmt
_explainExpression ty0 v0 =
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
-- enum LDST_res_t /function-name/(
--    struct LDST_cont_t *continuation,
--    void *closure,
--    struct LDST_t argument)
-- @
--
-- where @closure@ and @argument@ are only present for non-toplevel bindings,
-- including the curried forms of toplevel bindings.
functionSignature :: FunctionHeader -> (Builder, GenM Env, CVar (Pointer K))
functionSignature fun =
  maybe ([], pure mempty) (signatureParameters (funName fun) cClosureVar) (funArgs fun)
    & addContinuation
    & first (functionHeader retType (funName fun))
    & \(sig, mkBindings) -> (sig, mkBindings, cContVar)
  where
    retType = mconcat
      [ if funInternal fun then "static " else mempty
      , typeName @R Proxy
      ]

    addContinuation =
      _1 %~ (varDeclaration cContVar :)

    cContVar :: CVar (Pointer K)
    cContVar = CVar "_ldst_k"

    cClosureVar :: CVar (Pointer ())
    cClosureVar = CVar "_ldst_closure"

signatureParameters :: Builder -> CVar (Pointer ()) -> FunctionArgs -> ([Builder], GenM Env)
signatureParameters name voidClosure args = (params, bindings)
  where
    argVar = CVar @V $ identForC $ funArgIdent args
    params = [varDeclaration voidClosure, varDeclaration argVar]

    bindings = do
      let closureVar = cast @(Pointer V) voidClosure

      vars <- ifor (funClosure args) \i ident ->
        local (infoNameHint .~ identForC ident) do
          (ident,) <$> storeVar (accessI i closureVar)

      insertRecArg <- case funRecIdent args of
        Nothing -> pure id
        Just recId -> do
          -- It is possible that the recursion name recId shadows the functions
          -- argument name, but this follows the typechecker rules!
          -- 
          -- Use the following code to doublecheck:
          --
          --    val check = rec x (x : Int) : Int = x
          --
          -- If it typechecks 'recId' should *not* shadow an existing variable,
          -- if it fails to typecheck, it *should* shadow the variable.
          recVal <- mkValue TagLam . toCExp =<< mkLambda' name (unCVar voidClosure)
          pure $ Map.insert recId recVal

      pure
        $ insertRecArg
        $ Map.insert (funArgIdent args) argVar
        $ Map.fromList vars

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
  let (sig, mkBindings, initialK) = functionSignature (funHeader fun)

  let genBody = do
        bindings <- mkBindings
        local (infoBindings <>~ bindings) $
          generateExp (funBody fun)

  let info = Info
        { _infoBindings = mempty
        , _infoContinuation = initialK
        , _infoNameHint = funHint fun
        , _infoFuncHint = funName $ funHeader fun
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
    lam <- pushFunction 'l' (fv e) Nothing argId body
    mkValue TagLam lam
  e@(Rec recId argId _ _ body) -> do
    lam <- pushFunction 'r' (fv e) (Just recId) argId body
    mkValue TagLam lam
  Math m -> generateMath m
  Succ e -> do
    e' <- generateVal e
    liftValue TagInt $ access TagInt e' <> " + 1"
  Pair a b -> do
    a' <- generateVal a
    b' <- generateVal b
    mkValue TagPair (toCExp a', toCExp b')
  Fork e -> do
    let free = fv e
    lam <- pushFunction 'f' free Nothing (S.freshvar "unit" free) e
    forkLambda lam
    unit <- newUnitVar
    -- This unit value will probably not get used.
    tellStmt $ terminate $ unCVar $ cast @() unit
    pure unit
  New _ -> do
    chan <- mkValue TagChan . toCExp =<< newChannel
    mkValue TagPair (toCExp chan, toCExp chan)
  Send e -> do
    chan <- accessValChannel <$> generateVal e
    mkValue TagLam . toCExp =<< chanSendLambda chan

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
  Recv e mk -> do
    c <- accessValChannel <$> generateVal e
    k <- generateContinuationM mk
    chanReceive k c

pushFunction :: Char -> Set Ident -> Maybe Ident -> Ident -> Exp -> GenM (CExp L)
pushFunction c freevars mRecId argId body = do
  name <- fresh (Just c)
  closure <- mkClosure freevars
  hint <- view infoNameHint
  lift $ pushStack $ Function
    { funHeader = FunctionHeader
        { funName = name
        , funArgs = Just FunctionArgs
            { funClosure = closureVars closure
            , funArgIdent = argId
            , funRecIdent = mRecId
            }
        , funInternal = True
        }
    , funHint = hint
    , funBody = body
    }
  mkLambda name closure

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
        , funArgs = Just FunctionArgs
            { funClosure = closureVars closure
            , funArgIdent = resId
            , funRecIdent = Nothing
            }
        , funInternal = True
        }
    , funHint = hint
    , funBody = kbody
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
  invoke' fun k [closure, unCExp (toCExp val)]

invoke' :: ExpLike e => Builder -> e (Pointer K) -> [Builder] -> GenM ()
invoke' fun k args = tellStmt $ cReturn $ callExp fun $ unCExp (toCExp k) : args

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
  let var = CVar name
  tellStmt $ terminate $ varDeclaration var <+> B.char7 '=' <+> initExp
  pure var

varDeclaration :: forall t. CType t => CVar t -> Builder
varDeclaration (CVar v) = typeName @t Proxy <+> v

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
  expr <- cloneAll captureExprs
  pure Closure
    { closureVars = capturedVars
    , closureExpr = toCExp expr
    }

mkLambda :: Builder -> Closure -> GenM (CExp L)
mkLambda fun = fmap toCExp . mkLambda' fun . unCExp . closureExpr

mkLambda' :: Builder -> Builder -> GenM (CVar L)
mkLambda' fun closure = declareFresh $ braceList [fun, closure]

accessLambda :: CVar L -> (Builder, Builder)
accessLambda v = (accessRaw v "lam_fp", accessRaw v "lam_closure")

mkContinuation :: (ExpLike e1, ExpLike e2) => e1 L -> e2 (Pointer K) -> GenM (CVar K)
mkContinuation lambda next = declareFresh $ braceList [unCExp $ toCExp lambda, unCExp $ toCExp next]

accessContinuation :: CVar K -> (CVar L, CVar (Pointer K))
accessContinuation v = (CVar (accessRaw v "k_lam"), CVar (accessRaw v "k_next"))

accessPointer :: CVar (Pointer t) -> CVar t
accessPointer = accessI 0

accessI :: Int -> CVar (Pointer t) -> CVar t
accessI i (CVar v) = CVar $ v <> brackets (B.intDec i)

cast :: forall t' t. CType t' => CVar t -> CVar t'
cast (CVar v) = CVar $ parens $ parens (typeName @t' Proxy) <> v

takeAddress :: (ExpLike e, CType t) => e t -> GenM (CVar (Pointer t))
takeAddress = declareFresh . (B.char7 '&' <>) . unCExp . toCExp

mkValue :: Tag a -> a -> GenM (CVar V)
mkValue tag a = liftValue tag =<< case tag of
  TagInt -> pure $ B.intDec a
  TagLabel -> pure $ labelForC a
  TagPair -> do
    let (x, y) = a
    unCExp . toCExp <$> cloneAll [x, y]
  TagLam -> pure $ unCExp a
  TagChan -> pure $ unCExp a

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
clone = cloneAll . pure

-- | Clones a list of values, if the list is empty the null pointer is used.
cloneAll :: forall t e. (CType t, ExpLike e) => [e t] -> GenM (CVar (Pointer t))
cloneAll [] = storeVar nullPointer
cloneAll exprs = do
  let n = length exprs
  var <- declareFresh $ callExp funMalloc [B.intDec n <+> B.char7 '*' <+> cSizeof @t Proxy]
  itraverse_ (tellAssignI var) exprs
  pure var

nullPointer :: CExp (Pointer t)
nullPointer = CExp $ B.char7 '0'

-- | Glues the parts together to yield something looking like a function call.
-- It is also used to generate function headers and control structures.
callExp :: Builder -> [Builder] -> Builder
callExp f args = f <> parens (intercalate ", " args)

tellAssign :: ExpLike e => CVar t -> e t -> GenM ()
tellAssign (CVar v) val = tellStmt $ terminate $ bunwords [v, B.char7 '=', unCExp (toCExp val)]

tellAssignI :: ExpLike e => CVar (Pointer t) -> Int -> e t -> GenM ()
tellAssignI v idx = tellAssign (accessI idx v)

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
  , "#include \"LDST.h\""
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

cResType :: Builder
cResType = "enum LDST_res_t"

newChannel :: GenM (CVar (Pointer C))
newChannel = do
  chan <- storeVar nullPointer
  chanAddress <- takeAddress chan
  callChecked "ldst__chan_new" [unCVar chanAddress]
  pure chan

chanSendLambda :: ExpLike e => e (Pointer C) -> GenM (CVar L)
chanSendLambda = mkLambda' "ldst__chan_send" . unCExp . toCExp

chanReceive :: (ExpLike e1, ExpLike e2) => e1 (Pointer K) -> e2 (Pointer C) -> GenM ()
chanReceive (toCExp -> CExp k) (toCExp -> CExp chan) =
  tellStmt $ cReturn $ callExp "ldst__chan_recv" [k, chan]

forkLambda :: ExpLike e => e L -> GenM ()
forkLambda = callChecked "ldst__fork" . pure . unCExp . toCExp

callChecked :: Builder -> [Builder] -> GenM ()
callChecked fun args = do
  resVar <- local (infoNameHint .~ "res") do
    declareFresh @R $ callExp fun args
  returnNotOk resVar

returnNotOk :: CVar R -> GenM ()
returnNotOk (CVar res) = do
  tellStmt $ CStmt $ callExp "if " $ pure $ res <> " != LDST__ok"
  local (infoIndent +~ 1) $ tellStmt $ cReturn res

braceList :: [Builder] -> Builder
braceList bs = braces (intercalate ", " bs)

accessRaw :: CVar t -> Builder -> Builder
accessRaw (CVar v) x = v <> B.char7 '.' <> x

access :: Tag a -> CVar V -> Builder
access tag v = accessRaw v (tagAccessor tag)

accessPair :: CVar V -> (CVar V, CVar V)
accessPair v =
  let b = CVar $ access TagPair v :: CVar (Pointer V)
   in (accessI 0 b, accessI 1 b)

accessValLambda :: CVar V -> CVar L
accessValLambda = CVar . access TagLam

accessValChannel :: CVar V -> CVar (Pointer C)
accessValChannel = CVar . access TagChan

tagAccessor :: Tag a -> Builder
tagAccessor = \case
  TagInt   -> "val_int"
  TagLabel -> "val_label"
  TagPair  -> "val_pair"
  TagLam   -> "val_lam"
  TagChan  -> "val_chan"

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

cReturn :: Builder -> CStmt
cReturn b = terminate $ "return " <> b

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
functionForC idn = "ldst_" <> identForC idn

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
