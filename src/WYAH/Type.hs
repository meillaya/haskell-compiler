-- | Canonical types, kinds, predicates, and type schemes.
--
-- Surface type syntax lives in "WYAH.Syntax".  This module is the small,
-- normalized language used by the kind checker, the inferencer, and the
-- pretty-printer.  Type variables carry their kind so that higher-kinded
-- programs such as @data T f a = T (f a)@ can be checked.
module WYAH.Type
  ( Kind(..)
  , prettyKind
  , TVar(..)
  , freshTypeVariable
  , TyCon(..)
  , builtinTyCon
  , Type(..)
  , Pred(..)
  , Scheme(..)
  , schemeType
  , schemePredicates
  , mkFun
  , funArguments
  , funResult
  , ftvType
  , ftvPred
  , ftvScheme
  , ftvSchemeContext
  , applyType
  , applyPred
  , applyScheme
  , applyPredicates
  , composeSubstitution
  , normalizeType
  , normalizeScheme
  , normalizePred
  , normalizeKind
  , prettyType
  , prettyPred
  , prettyScheme
  , prettyContext
  , typeHead
  , typeArguments
  , isFunctionType
  ) where

import Data.List (nub)
import WYAH.Span (Span)

-- Kinds ---------------------------------------------------------------------

data Kind
  = KStar
  | KArrow Kind Kind
  | KPrim
  | KMeta Int
  deriving (Eq, Ord)

instance Show Kind where
  show = prettyKind

prettyKind :: Kind -> String
prettyKind kind = case kind of
  KStar -> "*"
  KPrim -> "#"
  KMeta n -> "?k" <> show n
  KArrow left right -> arrowLeft left <> " -> " <> prettyKind right
  where
    arrowLeft left = case left of
      KArrow _ _ -> "(" <> prettyKind left <> ")"
      _ -> prettyKind left

-- Type variables ------------------------------------------------------------

data TVar = TVar
  { tvId :: Int
  , tvName :: String
  , tvKind :: Kind
  , tvSkolem :: Bool
  }

instance Eq TVar where
  left == right = tvId left == tvId right

instance Ord TVar where
  compare left right = compare (tvId left) (tvId right)

instance Show TVar where
  show variable = tvName variable

freshTypeVariable :: Int -> String -> Kind -> TVar
freshTypeVariable uniqueId name kind =
  TVar
    { tvId = uniqueId
    , tvName = name
    , tvKind = kind
    , tvSkolem = False
    }

-- Type constructors ---------------------------------------------------------

data TyCon = TyCon
  { tyConName :: String
  , tyConKind :: Kind
  , tyConArity :: Int
  }
  deriving (Eq, Ord, Show)

builtinTyCon :: String -> Kind -> Int -> TyCon
builtinTyCon = TyCon

-- Types ---------------------------------------------------------------------

data Type
  = TVariable TVar
  | TCon TyCon
  | TApp Type Type
  | TFun Type Type

instance Eq Type where
  left == right = case (left, right) of
    (TVariable a, TVariable b) -> a == b
    (TCon a, TCon b) -> a == b
    (TApp a b, TApp c d) -> a == c && b == d
    (TFun a b, TFun c d) -> a == c && b == d
    _ -> False

instance Ord Type where
  compare left right = compare (show left) (show right)

instance Show Type where
  show = prettyType

data Pred = Pred
  { predSpan :: Span
  , predClass :: String
  , predType :: Type
  }

instance Eq Pred where
  left == right = predClass left == predClass right && predType left == predType right

instance Show Pred where
  show = prettyPred

data Scheme = Forall [TVar] [Pred] Type

instance Eq Scheme where
  left == right = case (left, right) of
    (Forall vars1 preds1 body1, Forall vars2 preds2 body2) ->
      vars1 == vars2 && preds1 == preds2 && body1 == body2

instance Show Scheme where
  show = prettyScheme

schemeType :: Scheme -> Type
schemeType (Forall _ _ body) = body

schemePredicates :: Scheme -> [Pred]
schemePredicates (Forall _ predicates _) = predicates

mkFun :: Type -> Type -> Type
mkFun = TFun

funArguments :: Type -> [Type]
funArguments (TFun argument result) = argument : funArguments result
funArguments _ = []

funResult :: Type -> Type
funResult (TFun _ result) = funResult result
funResult type_ = type_

isFunctionType :: Type -> Bool
isFunctionType (TFun _ _) = True
isFunctionType _ = False

typeHead :: Type -> Maybe String
typeHead (TCon constructor) = Just (tyConName constructor)
typeHead (TApp function _) = typeHead function
typeHead _ = Nothing

typeArguments :: Type -> [Type]
typeArguments (TApp function argument) = typeArguments function <> [argument]
typeArguments _ = []

-- Free variables ------------------------------------------------------------

ftvType :: Type -> [TVar]
ftvType type_ = nub (go type_)
  where
    go (TVariable variable) = [variable]
    go (TCon _) = []
    go (TApp function argument) = go function <> go argument
    go (TFun argument result) = go argument <> go result

ftvPred :: Pred -> [TVar]
ftvPred = ftvType . predType

ftvScheme :: Scheme -> [TVar]
ftvScheme (Forall _ predicates body) =
  nub (concatMap ftvPred predicates <> ftvType body)

ftvSchemeContext :: Scheme -> [TVar]
ftvSchemeContext (Forall _ predicates _) = nub (concatMap ftvPred predicates)

-- Substitution --------------------------------------------------------------

type Substitution = [(TVar, Type)]

applyType :: Substitution -> Type -> Type
applyType substitution type_ = case type_ of
  TVariable variable ->
    case lookup variable substitution of
      Just replacement -> applyType substitution replacement
      Nothing -> TVariable variable
  TCon constructor -> TCon constructor
  TApp function argument -> TApp (applyType substitution function) (applyType substitution argument)
  TFun argument result -> TFun (applyType substitution argument) (applyType substitution result)

applyPred :: Substitution -> Pred -> Pred
applyPred substitution predicate =
  predicate {predType = applyType substitution (predType predicate)}

applyPredicates :: Substitution -> [Pred] -> [Pred]
applyPredicates substitution = map (applyPred substitution)

applyScheme :: Substitution -> Scheme -> Scheme
applyScheme substitution (Forall variables predicates body) =
  let filtered = [(variable, replacement) | (variable, replacement) <- substitution, variable `notElem` variables]
   in Forall variables (applyPredicates filtered predicates) (applyType filtered body)

composeSubstitution :: Substitution -> Substitution -> Substitution
composeSubstitution newer older =
  [(variable, applyType newer replacement) | (variable, replacement) <- older] <> newer

-- Normalization -------------------------------------------------------------

normalizeType :: Type -> Type
normalizeType type_ = let (result, _, _) = go [] 0 type_ in result
  where
    go mapping nextId (TVariable variable) =
      case lookup (tvId variable) mapping of
        Just renamed ->
          (TVariable (TVar renamed ("t" <> show renamed) (normalizeKind (tvKind variable)) (tvSkolem variable)), mapping, nextId)
        Nothing ->
          let renamed = TVar nextId ("t" <> show nextId) (normalizeKind (tvKind variable)) (tvSkolem variable)
           in (TVariable renamed, (tvId variable, nextId) : mapping, nextId + 1)
    go mapping nextId (TCon constructor) = (TCon constructor, mapping, nextId)
    go mapping nextId (TApp function argument) =
      let (function', mapping', nextId') = go mapping nextId function
          (argument', mapping'', nextId'') = go mapping' nextId' argument
       in (TApp function' argument', mapping'', nextId'')
    go mapping nextId (TFun argument result) =
      let (argument', mapping', nextId') = go mapping nextId argument
          (result', mapping'', nextId'') = go mapping' nextId' result
       in (TFun argument' result', mapping'', nextId'')

normalizeScheme :: Scheme -> Scheme
normalizeScheme (Forall variables predicates body) =
  let mapping =
        [ (tvId variable, TVar newId ("t" <> show newId) (normalizeKind (tvKind variable)) (tvSkolem variable))
        | (variable, newId) <- zip variables [0 ..]
        ]
      renameType type_ = case type_ of
        TVariable variable ->
          case lookup (tvId variable) mapping of
            Just replacement -> TVariable replacement
            Nothing -> TVariable variable
        TCon constructor -> TCon constructor
        TApp function argument -> TApp (renameType function) (renameType argument)
        TFun argument result -> TFun (renameType argument) (renameType result)
   in Forall
        (map snd mapping)
        [ predicate {predType = renameType (predType predicate)}
        | predicate <- predicates
        ]
        (renameType body)

normalizePred :: Pred -> Pred
normalizePred predicate = predicate {predType = normalizeType (predType predicate)}

normalizeKind :: Kind -> Kind
normalizeKind kind = case kind of
  KArrow left right -> KArrow (normalizeKind left) (normalizeKind right)
  KMeta n -> KMeta n
  other -> other

-- Pretty printing -----------------------------------------------------------

prettyType :: Type -> String
prettyType type_ = case type_ of
  TVariable variable -> tvName variable
  TCon constructor -> tyConName constructor
  TApp (TCon listConstructor) element
    | tyConName listConstructor == "[]" -> "[" <> prettyType element <> "]"
  TApp (TApp (TCon tupleConstructor) first) second
    | tyConName tupleConstructor == "(,)" -> "(" <> prettyType first <> ", " <> prettyType second <> ")"
  TApp function argument ->
    prettyApplicationFunction function <> " " <> prettyApplicationArgument argument
  TFun argument result ->
    prettyFunctionArgument argument <> " -> " <> prettyType result
  where
    prettyFunctionArgument argument = case argument of
      TFun _ _ -> "(" <> prettyType argument <> ")"
      _ -> prettyType argument
    prettyApplicationFunction function = case function of
      TFun _ _ -> "(" <> prettyType function <> ")"
      _ -> prettyType function
    prettyApplicationArgument argument = case argument of
      TFun _ _ -> "(" <> prettyType argument <> ")"
      TApp _ _ -> "(" <> prettyType argument <> ")"
      _ -> prettyType argument

prettyPred :: Pred -> String
prettyPred predicate = predClass predicate <> " " <> prettyPredicateArgument (predType predicate)
  where
    prettyPredicateArgument type_ = case type_ of
      TFun _ _ -> "(" <> prettyType type_ <> ")"
      TApp _ _ -> "(" <> prettyType type_ <> ")"
      _ -> prettyType type_

prettyContext :: [Pred] -> String
prettyContext [] = ""
prettyContext [predicate] = prettyPred predicate <> " => "
prettyContext predicates = "(" <> commaSeparated (map prettyPred predicates) <> ") => "

prettyScheme :: Scheme -> String
prettyScheme scheme@(Forall variables predicates body)
  | null variables && null predicates = prettyType body
  | otherwise =
      case normalizeScheme scheme of
        Forall normalizedVariables normalizedPredicates normalizedBody ->
          let forallPart =
                if null normalizedVariables
                  then ""
                  else "forall " <> unwords (map tvName normalizedVariables) <> ". "
           in forallPart <> prettyContext normalizedPredicates <> prettyType normalizedBody

commaSeparated :: [String] -> String
commaSeparated [] = ""
commaSeparated [value] = value
commaSeparated (value : rest) = value <> ", " <> commaSeparated rest
