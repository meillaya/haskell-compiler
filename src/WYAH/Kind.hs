-- | Kind inference and canonicalization for ProtoHaskell.
--
-- The checker does three jobs that all depend on the same environment:
--
-- * expand type synonyms (surface syntax does not have to know about them),
-- * infer kinds for data declarations, including higher-kinded parameters,
-- * convert surface types, class constraints, and signatures into the
--   canonical representation used by the inferencer.
module WYAH.Kind
  ( KindError(..)
  , TypeInfo(..)
  , DataDef(..)
  , ConDef(..)
  , ClassDef(..)
  , InstanceDef(..)
  , CheckedModule(..)
  , checkModule
  , builtinDataDefs
  , builtinTypeInfos
  , constructorScheme
  , lookupTypeInfo
  , kindOfTypeText
  ) where

import Data.List (nub, (\\))
import WYAH.Diagnostic
import WYAH.Prelude (builtinName)
import WYAH.Renamed
import WYAH.Span
import WYAH.Syntax (Name, typeSpan)
import qualified WYAH.Syntax as S
import WYAH.Type

data KindError = KindError
  { kindErrorSource :: FilePath
  , kindErrorSpan :: Span
  , kindErrorMessage :: String
  }
  deriving (Eq)

instance Show KindError where
  show err =
    renderDiagnostic
      Diagnostic
        { diagnosticPhase = phaseName KindPhase
        , diagnosticSource = Just (kindErrorSource err)
        , diagnosticSpan = Just (kindErrorSpan err)
        , diagnosticMessage = kindErrorMessage err
        }

data TypeInfo = TypeInfo
  { typeInfoKind :: Kind
  , typeInfoArity :: Int
  }
  deriving (Eq, Show)

data DataDef = DataDef
  { dataDefName :: ResolvedName
  , dataDefTypeCon :: TyCon
  , dataDefVars :: [TVar]
  , dataDefCons :: [ConDef]
  , dataDefDeriving :: [Name]
  }
  deriving (Eq, Show)

data ConDef = ConDef
  { conDefName :: ResolvedName
  , conDefFields :: [Type]
  , conDefResult :: Type
  }
  deriving (Eq, Show)

data ClassDef = ClassDef
  { classDefName :: Name
  , classDefVar :: TVar
  , classDefSupers :: [Pred]
  , classDefMethods :: [(ResolvedName, Scheme)]
  }
  deriving (Eq, Show)

data InstanceDef = InstanceDef
  { instanceDefClass :: Name
  , instanceDefHead :: Type
  , instanceDefContext :: [Pred]
  , instanceDefVars :: [TVar]
  , instanceDefMethods :: [(ResolvedName, [RMatch])]
  , instanceDefSpan :: Span
  }
  deriving (Eq, Show)

data CheckedModule = CheckedModule
  { checkedModule :: RModule
  , checkedDataDefs :: [DataDef]
  , checkedClassDefs :: [ClassDef]
  , checkedInstanceDefs :: [InstanceDef]
  , checkedConstructorSchemes :: [(ResolvedName, Scheme)]
  , checkedSignatures :: [(ResolvedName, Scheme)]
  , checkedTypeInfos :: [(Name, TypeInfo)]
  }
  deriving (Eq, Show)

-- Kind checker monad --------------------------------------------------------

data KindState = KindState
  { ksSource :: FilePath
  , ksNext :: Int
  , ksSubstitution :: [(Int, Kind)]
  , ksTypeInfos :: [(Name, TypeInfo)]
  , ksClasses :: [(Name, Int)]
  }

newtype KindM a = KindM {runKindM :: KindState -> Either KindError (a, KindState)}

instance Functor KindM where
  fmap f (KindM action) = KindM $ \state ->
    case action state of
      Left err -> Left err
      Right (value, state') -> Right (f value, state')

instance Applicative KindM where
  pure value = KindM $ \state -> Right (value, state)
  KindM pf <*> KindM pa = KindM $ \state ->
    case pf state of
      Left err -> Left err
      Right (f, state') ->
        case pa state' of
          Left err -> Left err
          Right (value, state'') -> Right (f value, state'')

instance Monad KindM where
  KindM action >>= f = KindM $ \state ->
    case action state of
      Left err -> Left err
      Right (value, state') -> runKindM (f value) state'

getKindState :: KindM KindState
getKindState = KindM $ \state -> Right (state, state)

modifyKindState :: (KindState -> KindState) -> KindM ()
modifyKindState f = KindM $ \state -> Right ((), f state)

failKind :: Span -> String -> KindM a
failKind span_ message = KindM $ \state ->
  Left
    KindError
      { kindErrorSource = ksSource state
      , kindErrorSpan = span_
      , kindErrorMessage = message
      }

freshKindMeta :: KindM Kind
freshKindMeta = do
  state <- getKindState
  let uniqueId = ksNext state
  modifyKindState $ \current -> current {ksNext = uniqueId + 1}
  pure (KMeta uniqueId)

lookupKindSubstitution :: Int -> KindM (Maybe Kind)
lookupKindSubstitution uniqueId = do
  state <- getKindState
  pure (lookup uniqueId (ksSubstitution state))

resolveKind :: Kind -> KindM Kind
resolveKind kind = case kind of
  KMeta uniqueId -> do
    replacement <- lookupKindSubstitution uniqueId
    case replacement of
      Just kind' -> resolveKind kind'
      Nothing -> pure (KMeta uniqueId)
  KArrow left right -> KArrow <$> resolveKind left <*> resolveKind right
  other -> pure other

unifyKind :: Span -> Kind -> Kind -> KindM ()
unifyKind span_ left right = do
  left' <- resolveKind left
  right' <- resolveKind right
  case (left', right') of
    (KMeta uniqueId, kind) -> bindKindMeta span_ uniqueId kind
    (kind, KMeta uniqueId) -> bindKindMeta span_ uniqueId kind
    (KStar, KStar) -> pure ()
    (KPrim, KPrim) -> pure ()
    (KArrow leftArgument leftResult, KArrow rightArgument rightResult) -> do
      unifyKind span_ leftArgument rightArgument
      unifyKind span_ leftResult rightResult
    _ ->
      failKind
        span_
        ("kind mismatch: cannot unify " <> prettyKind left' <> " with " <> prettyKind right')

bindKindMeta :: Span -> Int -> Kind -> KindM ()
bindKindMeta span_ uniqueId kind = do
  kind' <- resolveKind kind
  if kind' == KMeta uniqueId
    then pure ()
    else do
      occurs <- kindOccurs uniqueId kind'
      if occurs
        then failKind span_ "occurs check failed while unifying kinds"
        else modifyKindState $ \state ->
          state {ksSubstitution = (uniqueId, kind') : ksSubstitution state}

kindOccurs :: Int -> Kind -> KindM Bool
kindOccurs uniqueId kind = case kind of
  KMeta other -> pure (uniqueId == other)
  KArrow left right -> (||) <$> kindOccurs uniqueId left <*> kindOccurs uniqueId right
  _ -> pure False

zonkKind :: Kind -> KindM Kind
zonkKind = resolveKind

zonkType :: Type -> KindM Type
zonkType type_ = case type_ of
  TVariable variable -> do
    kind <- zonkKind (tvKind variable)
    pure (TVariable variable {tvKind = kind})
  TCon constructor -> do
    kind <- zonkKind (tyConKind constructor)
    pure (TCon constructor {tyConKind = kind})
  TApp function argument -> TApp <$> zonkType function <*> zonkType argument
  TFun argument result -> TFun <$> zonkType argument <*> zonkType result

zonkPredicate :: Pred -> KindM Pred
zonkPredicate predicate = do
  type_ <- zonkType (predType predicate)
  pure predicate {predType = type_}

zonkScheme :: Scheme -> KindM Scheme
zonkScheme (Forall variables predicates body) = do
  variables' <- mapM zonkVariable variables
  predicates' <- mapM zonkPredicate predicates
  body' <- zonkType body
  pure (Forall variables' predicates' body')
  where
    zonkVariable variable = do
      kind <- zonkKind (tvKind variable)
      pure variable {tvKind = kind}

zonkDataDef :: DataDef -> KindM DataDef
zonkDataDef definition = do
  variables <- mapM zonkVariable (dataDefVars definition)
  constructors <- mapM zonkConstructor (dataDefCons definition)
  typeConKind' <- zonkKind (tyConKind (dataDefTypeCon definition))
  pure
    definition
      { dataDefTypeCon = (dataDefTypeCon definition) {tyConKind = typeConKind'}
      , dataDefVars = variables
      , dataDefCons = constructors
      }
  where
    zonkVariable variable = do
      kind <- zonkKind (tvKind variable)
      pure variable {tvKind = kind}
    zonkConstructor constructor = do
      fields <- mapM zonkType (conDefFields constructor)
      result <- zonkType (conDefResult constructor)
      pure constructor {conDefFields = fields, conDefResult = result}

-- Initial environment -------------------------------------------------------

builtinTypeInfos :: [(Name, TypeInfo)]
builtinTypeInfos =
  [ ("Int", TypeInfo KStar 0)
  , ("Bool", TypeInfo KStar 0)
  , ("Char", TypeInfo KStar 0)
  , ("()", TypeInfo KStar 0)
  , ("[]", TypeInfo (KArrow KStar KStar) 1)
  , ("(,)", TypeInfo (KArrow KStar (KArrow KStar KStar)) 2)
  , ("(,,)", TypeInfo (KArrow KStar (KArrow KStar (KArrow KStar KStar))) 3)
  , ("(,,,)", TypeInfo (KArrow KStar (KArrow KStar (KArrow KStar (KArrow KStar KStar)))) 4)
  , ("(->)", TypeInfo (KArrow KStar (KArrow KStar KStar)) 2)
  , ("IO", TypeInfo (KArrow KStar KStar) 1)
  , ("Maybe", TypeInfo (KArrow KStar KStar) 1)
  , ("Either", TypeInfo (KArrow KStar (KArrow KStar KStar)) 2)
  , ("Int#", TypeInfo KPrim 0)
  , ("Char#", TypeInfo KPrim 0)
  , ("Double#", TypeInfo KPrim 0)
  , ("Addr#", TypeInfo KPrim 0)
  ]

lookupTypeInfo :: Name -> KindM TypeInfo
lookupTypeInfo name = do
  state <- getKindState
  case lookup name (ksTypeInfos state) of
    Just info -> pure info
    Nothing -> failKind NoSpan ("type constructor not in scope: " <> name)

-- Type synonym expansion ----------------------------------------------------

type SynonymMap = [(Name, ([Name], S.Type))]

collectSynonyms :: RModule -> KindM SynonymMap
collectSynonyms rModule = do
  let userSynonyms =
        [ (typeSynInfoName info, (typeSynInfoVars info, typeSynInfoRhs info))
        | RTypeSynDecl info <- rModuleDecls rModule
        ]
      builtinSynonym = [("String", ([], S.TyList NoSpan (S.TyCon NoSpan "Char")))]
      synonyms = userSynonyms <> builtinSynonym
  mapM_ (validateSynonym synonyms) userSynonyms
  pure synonyms

validateSynonym :: SynonymMap -> (Name, ([Name], S.Type)) -> KindM ()
validateSynonym synonyms (name, (variables, rhs)) = do
  let unknownVariables =
        [ variable
        | variable <- surfaceTypeVariables rhs
        , variable `notElem` variables
        ]
  case unknownVariables of
    variable : _ -> failKind (typeSpan rhs) ("unknown type variable in synonym " <> name <> ": " <> variable)
    [] -> pure ()
  detectCycle [name] rhs
  where
    detectCycle stack type_ = case type_ of
      S.TyCon _ constructor
        | Just (_, synonymRhs) <- lookup constructor synonyms ->
            if constructor `elem` stack
              then failKind (typeSpan type_) ("recursive type synonym: " <> constructor)
              else detectCycle (constructor : stack) synonymRhs
      S.TyApp _ function argument -> detectCycle stack function >> detectCycle stack argument
      S.TyFun _ argument result -> detectCycle stack argument >> detectCycle stack result
      S.TyForall _ _ body -> detectCycle stack body
      S.TyContext _ predicates body -> mapM_ (detectCycle stack . S.predType) predicates >> detectCycle stack body
      S.TyTuple _ types -> mapM_ (detectCycle stack) types
      S.TyList _ element -> detectCycle stack element
      _ -> pure ()

expandSynonyms :: SynonymMap -> S.Type -> KindM S.Type
expandSynonyms synonyms = go 0 []
  where
    go depth stack type_ =
      if depth > 200
        then failKind (typeSpan type_) "type synonym expansion exceeded the recursion limit"
        else case type_ of
          S.TyCon _ name
            | Just (parameters, rhs) <- lookup name synonyms ->
                if null parameters
                  then go (depth + 1) (name : stack) rhs
                  else failKind (typeSpan type_) ("type synonym " <> name <> " is used with too few arguments")
          S.TyApp _ _ _ ->
            let (headType, arguments) = flattenApplication type_
             in case headType of
                  S.TyCon _ name
                    | Just (parameters, rhs) <- lookup name synonyms ->
                        if length arguments < length parameters
                          then failKind (typeSpan type_) ("type synonym " <> name <> " is used with too few arguments")
                          else do
                            expandedArguments <- mapM (go depth stack) arguments
                            substituted <-
                              expandSubstituted
                                depth
                                (name : stack)
                                parameters
                                (take (length parameters) expandedArguments)
                                rhs
                            go (depth + 1) stack (applySurfaceArguments substituted (drop (length parameters) expandedArguments))
                  _ -> do
                    expandedHead <- go depth stack headType
                    expandedArguments <- mapM (go depth stack) arguments
                    pure (foldl (\function argument -> S.TyApp (typeSpan type_) function argument) expandedHead expandedArguments)
          S.TyFun _ argument result -> S.TyFun (typeSpan type_) <$> go depth stack argument <*> go depth stack result
          S.TyList _ element -> S.TyList (typeSpan type_) <$> go depth stack element
          S.TyTuple _ types -> S.TyTuple (typeSpan type_) <$> mapM (go depth stack) types
          S.TyForall _ variables body -> S.TyForall (typeSpan type_) variables <$> go depth stack body
          S.TyContext _ predicates body -> do
            predicates' <- mapM expandPredicate predicates
            body' <- go depth stack body
            pure (S.TyContext (typeSpan type_) predicates' body')
          other -> pure other

    expandPredicate predicate = do
      type_ <- go 0 [] (S.predType predicate)
      pure (S.Pred (S.predSpan predicate) (S.predClass predicate) type_)

    expandSubstituted depth stack parameters arguments rhs = do
      let substitution = zip parameters arguments
          substituted = substituteSurface substitution rhs
      if any (`elem` stack) (surfaceTypeConstructors substituted)
        then failKind (typeSpan rhs) "recursive type synonym"
        else go (depth + 1) stack substituted

    applySurfaceArguments type_ arguments =
      foldl (\function argument -> S.TyApp (typeSpan function) function argument) type_ arguments

surfaceTypeVariables :: S.Type -> [Name]
surfaceTypeVariables type_ = nub (go type_)
  where
    go (S.TyVar _ name) = [name]
    go (S.TyCon _ _) = []
    go (S.TyApp _ function argument) = go function <> go argument
    go (S.TyFun _ argument result) = go argument <> go result
    go (S.TyForall _ variables body) = filter (`notElem` variables) (go body)
    go (S.TyContext _ predicates body) = concatMap (go . S.predType) predicates <> go body
    go (S.TyTuple _ types) = concatMap go types
    go (S.TyList _ element) = go element

surfaceTypeConstructors :: S.Type -> [Name]
surfaceTypeConstructors type_ = nub (go type_)
  where
    go (S.TyCon _ name) = [name]
    go (S.TyApp _ function argument) = go function <> go argument
    go (S.TyFun _ argument result) = go argument <> go result
    go (S.TyForall _ _ body) = go body
    go (S.TyContext _ predicates body) = concatMap (go . S.predType) predicates <> go body
    go (S.TyTuple _ types) = concatMap go types
    go (S.TyList _ element) = go element
    go (S.TyVar _ _) = []

substituteSurface :: [(Name, S.Type)] -> S.Type -> S.Type
substituteSurface substitution type_ = case type_ of
  S.TyCon _ name -> case lookup name substitution of
    Just replacement -> replacement
    Nothing -> type_
  S.TyVar _ name -> case lookup name substitution of
    Just replacement -> replacement
    Nothing -> type_
  S.TyApp _ function argument -> S.TyApp (typeSpan type_) (substituteSurface substitution function) (substituteSurface substitution argument)
  S.TyFun _ argument result -> S.TyFun (typeSpan type_) (substituteSurface substitution argument) (substituteSurface substitution result)
  S.TyForall _ variables body -> S.TyForall (typeSpan type_) variables (substituteSurface (filter ((`notElem` variables) . fst) substitution) body)
  S.TyContext _ predicates body ->
    S.TyContext
      (typeSpan type_)
      [ S.Pred
          (S.predSpan predicate)
          (S.predClass predicate)
          (substituteSurface substitution (S.predType predicate))
      | predicate <- predicates
      ]
      (substituteSurface substitution body)
  S.TyTuple _ types -> S.TyTuple (typeSpan type_) (map (substituteSurface substitution) types)
  S.TyList _ element -> S.TyList (typeSpan type_) (substituteSurface substitution element)

flattenApplication :: S.Type -> (S.Type, [S.Type])
flattenApplication type_ = go type_ []
  where
    go (S.TyApp _ function argument) arguments = go function (argument : arguments)
    go headType arguments = (headType, arguments)

-- Type and predicate conversion --------------------------------------------

type VariableMap = [(Name, TVar)]

convertType :: VariableMap -> S.Type -> KindM (Kind, Type)
convertType variables type_ = case type_ of
  S.TyCon span_ name -> do
    info <- lookupTypeInfo name
    pure
      ( typeInfoKind info
      , TCon (TyCon name (typeInfoKind info) (typeInfoArity info))
      )
  S.TyVar span_ name ->
    case lookup name variables of
      Just variable -> pure (tvKind variable, TVariable variable)
      Nothing -> failKind span_ ("type variable not in scope: " <> name)
  S.TyApp span_ function argument -> do
    (functionKind, functionType) <- convertType variables function
    (argumentKind, argumentType) <- convertType variables argument
    resultKind <- freshKindMeta
    unifyKind span_ functionKind (KArrow argumentKind resultKind)
    pure (resultKind, TApp functionType argumentType)
  S.TyFun span_ argument result -> do
    (_, argumentType) <- convertType variables argument
    (_, resultType) <- convertType variables result
    unifyKind span_ (kindOfType argumentType) KStar
    unifyKind span_ (kindOfType resultType) KStar
    pure (KStar, TFun argumentType resultType)
  S.TyList span_ element -> do
    (_, elementType) <- convertType variables element
    unifyKind span_ (kindOfType elementType) KStar
    listInfo <-
      case lookup "[]" ([(name, info) | (name, info) <- builtinTypeInfos]) of
        Just info -> pure info
        Nothing -> failKind span_ "internal kind error: missing list type constructor"
    pure (KStar, TApp (TCon (TyCon "[]" (typeInfoKind listInfo) 1)) elementType)
  S.TyTuple span_ types -> do
    converted <- mapM (convertType variables) types
    let tupleName = "(" <> replicate (length types - 1) ',' <> ")"
    case lookup tupleName builtinTypeInfos of
      Nothing -> failKind span_ ("unsupported tuple arity: " <> show (length types))
      Just info -> do
        mapM_ (\(_, type_) -> unifyKind span_ (kindOfType type_) KStar) converted
        let constructor = TCon (TyCon tupleName (typeInfoKind info) (typeInfoArity info))
            applied = foldl TApp constructor (map snd converted)
        pure (KStar, applied)
  S.TyForall span_ _ _ ->
    failKind span_ "forall is only allowed at the top level of a type signature"
  S.TyContext span_ _ _ ->
    failKind span_ "a type context is only allowed at the top level of a type signature"

kindOfType :: Type -> Kind
kindOfType type_ = case type_ of
  TVariable variable -> tvKind variable
  TCon constructor -> tyConKind constructor
  TApp function _ -> case kindOfType function of
    KArrow _ result -> result
    _ -> KStar
  TFun _ _ -> KStar

convertPredicate :: VariableMap -> S.Pred -> KindM Pred
convertPredicate variables predicate = do
  state <- getKindState
  case lookup (S.predClass predicate) (ksClasses state) of
    Nothing -> failKind (S.predSpan predicate) ("class not in scope: " <> S.predClass predicate)
    Just arity
      | arity /= 1 -> failKind (S.predSpan predicate) "only single-parameter classes are supported"
      | otherwise -> do
          (kind, type_) <- convertType variables (S.predType predicate)
          unifyKind (S.predSpan predicate) kind KStar
          pure (Pred (S.predSpan predicate) (S.predClass predicate) type_)

flattenSignature :: S.Type -> ([Name], [S.Pred], S.Type)
flattenSignature type_ = case type_ of
  S.TyForall _ variables body ->
    let (moreVariables, predicates, result) = flattenSignature body
     in (variables <> moreVariables, predicates, result)
  S.TyContext _ predicates body ->
    let (variables, morePredicates, result) = flattenSignature body
     in (variables, predicates <> morePredicates, result)
  other -> ([], [], other)

convertScheme :: VariableMap -> S.Type -> KindM Scheme
convertScheme outerVariables signature = do
  let (forallVariables, predicates, body) = flattenSignature signature
      freeVariables = surfaceTypeVariables signature
      allVariables = nub (forallVariables <> freeVariables)
  variables <- mapM makeVariable allVariables
  let variableMap = variables <> outerVariables
  convertedPredicates <- mapM (convertPredicate variableMap) predicates
  (bodyKind, convertedBody) <- convertType variableMap body
  unifyKind (typeSpan body) bodyKind KStar
  quantified <- filterMUsed (map snd variables) convertedPredicates convertedBody
  scheme <- zonkScheme (Forall quantified convertedPredicates convertedBody)
  pure scheme
  where
    makeVariable name = do
      kind <- freshKindMeta
      uniqueId <- freshVariableId
      pure (name, freshTypeVariable uniqueId name kind)
    freshVariableId = do
      state <- getKindState
      modifyKindState $ \current -> current {ksNext = ksNext current + 1}
      pure (ksNext state)
    filterMUsed variables predicates body = do
      let used = nub (concatMap ftvPred predicates <> ftvType body)
      pure [variable | variable <- variables, variable `elem` used]

-- Module checking -----------------------------------------------------------

kindOfTypeText :: FilePath -> CheckedModule -> S.Type -> Either KindError Kind
kindOfTypeText file checked surfaceType = do
  let initial =
        KindState
          { ksSource = file
          , ksNext = 1000000
          , ksSubstitution = []
          , ksTypeInfos = checkedTypeInfos checked
          , ksClasses = [(classDefName definition, 1) | definition <- checkedClassDefs checked]
          }
  (kind, _) <-
    runKindM
      ( do
          synonyms <- collectSynonyms (checkedModule checked)
          kindOfTypeM synonyms surfaceType
      )
      initial
  pure kind

kindOfTypeM :: SynonymMap -> S.Type -> KindM Kind
kindOfTypeM synonyms surfaceType = do
  expanded <- expandSynonyms synonyms surfaceType
  let names = nub (surfaceTypeVariables expanded)
  variables <- mapM makeVariable names
  (kind, _) <- convertType variables expanded
  defaultKindMetas
  zonkKind kind
  where
    makeVariable name = do
      kind <- freshKindMeta
      uniqueId <- freshKindVariableId
      pure (name, freshTypeVariable uniqueId name kind)

zonkTypeInfo :: (Name, TypeInfo) -> KindM (Name, TypeInfo)
zonkTypeInfo (name, info) = do
  kind <- zonkKind (typeInfoKind info)
  pure (name, info {typeInfoKind = kind})

checkModule :: FilePath -> RModule -> Either KindError CheckedModule
checkModule file rModule = do
  let initial =
        KindState
          { ksSource = file
          , ksNext = 1000000
          , ksSubstitution = []
          , ksTypeInfos = builtinTypeInfos
          , ksClasses = []
          }
  (checked, _) <- runKindM (checkModuleM rModule) initial
  pure checked

builtinDataInfos :: [(Name, TypeInfo)]
builtinDataInfos =
  [ (name, typeInfo)
  | definition <- builtinDataDefs
  , let name = resolvedOriginal (dataDefName definition)
        typeInfo = TypeInfo (tyConKind (dataDefTypeCon definition)) (tyConArity (dataDefTypeCon definition))
  ]

defaultKindMetas :: KindM ()
defaultKindMetas = do
  state <- getKindState
  let bound = map fst (ksSubstitution state)
      unbound = [uniqueId | uniqueId <- [0 .. ksNext state - 1], uniqueId `notElem` bound]
  modifyKindState $ \current ->
    current {ksSubstitution = [(uniqueId, KStar) | uniqueId <- unbound] <> ksSubstitution current}

checkModuleM :: RModule -> KindM CheckedModule
checkModuleM rModule = do
  synonyms <- collectSynonyms rModule
  dataDeclarations <- predeclareDataTypes rModule
  checkTypeSynonyms synonyms
  dataDefs <- mapM (checkDataDeclaration synonyms) dataDeclarations
  predeclareClasses rModule
  classDefs <- mapM (checkClassDeclaration synonyms) [(info, span_) | (span_, info) <- classInfos rModule]
  instanceDefs <- mapM (checkInstanceDeclaration synonyms) [(info, span_) | (span_, info) <- instanceInfos rModule]
  signatureSchemes <- checkSignatures synonyms rModule
  defaultKindMetas
  dataDefs' <- mapM zonkDataDef dataDefs
  classDefs' <- mapM zonkClassDef classDefs
  instanceDefs' <- mapM zonkInstanceDef instanceDefs
  case duplicateInstanceKey instanceDefs' of
    Just key -> failKind NoSpan ("duplicate instance declaration for " <> key)
    Nothing -> pure ()
  signatureSchemes' <- mapM zonkNamedScheme signatureSchemes
  let builtinConstructorSchemes =
        [ (conDefName constructor, constructorScheme definition constructor)
        | definition <- builtinDataDefs
        , constructor <- dataDefCons definition
        ]
      userConstructorSchemes =
        [ (conDefName constructor, constructorScheme definition constructor)
        | definition <- dataDefs'
        , constructor <- dataDefCons definition
        ]
  state <- getKindState
  typeInfos <- mapM zonkTypeInfo (ksTypeInfos state)
  pure
    CheckedModule
      { checkedModule = rModule
      , checkedDataDefs = builtinDataDefs <> dataDefs'
      , checkedClassDefs = classDefs'
      , checkedInstanceDefs = instanceDefs'
      , checkedConstructorSchemes = builtinConstructorSchemes <> userConstructorSchemes
      , checkedSignatures = signatureSchemes'
      , checkedTypeInfos = typeInfos
      }

classInfos :: RModule -> [(Span, ClassInfo)]
classInfos rModule =
  [ (classInfoSpan info, info)
  | RClassDecl info <- rModuleDecls rModule
  ]

instanceInfos :: RModule -> [(Span, InstanceInfo)]
instanceInfos rModule =
  [ (instanceInfoSpan info, info)
  | RInstanceDecl info <- rModuleDecls rModule
  ]

data DataPredecl = DataPredecl
  { predeclInfo :: DataInfo
  , predeclTypeCon :: TyCon
  , predeclVariables :: [TVar]
  }

predeclareDataTypes :: RModule -> KindM [DataPredecl]
predeclareDataTypes rModule = mapM predeclare infos
  where
    infos = [info | RDataDecl info <- rModuleDecls rModule]
    predeclare info = do
      variables <- mapM makeVariable (dataInfoVars info)
      let kind = foldr KArrow KStar (map tvKind variables)
          typeConstructor = TyCon (resolvedOriginal (dataInfoName info)) kind (length variables)
      modifyKindState $ \state ->
        state
          { ksTypeInfos =
              (resolvedOriginal (dataInfoName info), TypeInfo kind (length variables)) : ksTypeInfos state
          }
      pure
        DataPredecl
          { predeclInfo = info
          , predeclTypeCon = typeConstructor
          , predeclVariables = variables
          }
    makeVariable name = do
      kind <- freshKindMeta
      uniqueId <- freshKindVariableId
      pure (freshTypeVariable uniqueId name kind)

freshKindVariableId :: KindM Int
freshKindVariableId = do
  state <- getKindState
  let uniqueId = ksNext state
  modifyKindState $ \current -> current {ksNext = uniqueId + 1}
  pure uniqueId

checkTypeSynonyms :: SynonymMap -> KindM ()
checkTypeSynonyms synonyms =
  mapM_ checkUserSynonym userSynonyms
  where
    userSynonyms = [(name, parameters, rhs) | (name, (parameters, rhs)) <- synonyms, name /= "String"]
    checkUserSynonym (name, parameters, rhs) = do
      expanded <- expandSynonyms synonyms rhs
      variables <- mapM makeVariable parameters
      (_, _) <- convertType variables expanded
      pure ()
    makeVariable name = do
      kind <- freshKindMeta
      uniqueId <- freshKindVariableId
      pure (name, freshTypeVariable uniqueId name kind)

checkDataDeclaration :: SynonymMap -> DataPredecl -> KindM DataDef
checkDataDeclaration synonyms predecl = do
  let variables = [(tvName variable, variable) | variable <- predeclVariables predecl]
      dataName = resolvedOriginal (dataInfoName (predeclInfo predecl))
      expectedResult =
        foldl TApp (TCon (predeclTypeCon predecl)) (map TVariable (predeclVariables predecl))
  constructors <- mapM (checkConstructor variables expectedResult dataName) (dataInfoCons (predeclInfo predecl))
  pure
    DataDef
      { dataDefName = dataInfoName (predeclInfo predecl)
      , dataDefTypeCon = predeclTypeCon predecl
      , dataDefVars = predeclVariables predecl
      , dataDefCons = constructors
      , dataDefDeriving = dataInfoDeriving (predeclInfo predecl)
      }
  where
    checkConstructor variables expectedResult dataName constructor = do
      fields <- mapM (convertField variables) (conInfoFields constructor)
      result <-
        case conInfoResult constructor of
          Nothing -> pure expectedResult
          Just resultType -> do
            expanded <- expandSynonyms synonyms resultType
            (kind, converted) <- convertType variables expanded
            unifyKind (conInfoSpan constructor) kind KStar
            unifyKind (conInfoSpan constructor) (kindOfType converted) (kindOfType expectedResult)
            case typeHead converted of
              Just headName
                | headName == dataName -> pure ()
              _ ->
                failKind
                  (conInfoSpan constructor)
                  ("constructor result must be " <> dataName)
            pure converted
      pure
        ConDef
          { conDefName = conInfoName constructor
          , conDefFields = fields
          , conDefResult = result
          }
    convertField variables field = do
      expanded <- expandSynonyms synonyms field
      (kind, converted) <- convertType variables expanded
      unifyKind (typeSpan field) kind KStar
      pure converted

predeclareClasses :: RModule -> KindM ()
predeclareClasses rModule =
  mapM_ predeclare (classInfos rModule)
  where
    predeclare (_, info) =
      modifyKindState $ \state ->
        state {ksClasses = (classInfoName info, 1) : ksClasses state}

checkClassDeclaration :: SynonymMap -> (ClassInfo, Span) -> KindM ClassDef
checkClassDeclaration synonyms (info, span_) = do
  uniqueId <- freshKindVariableId
  let classVariable = freshTypeVariable uniqueId (classInfoVar info) KStar
      variables = [(classInfoVar info, classVariable)]
  supers <- mapM (convertPredicate variables) (classInfoSupers info)
  methods <- mapM (checkMethod variables classVariable) (classInfoMethods info)
  pure
    ClassDef
      { classDefName = classInfoName info
      , classDefVar = classVariable
      , classDefSupers = supers
      , classDefMethods = methods
      }
  where
    checkMethod variables classVariable method = do
      expanded <- expandSynonyms synonyms (classMethodType method)
      let (forallVariables, predicates, body) = flattenSignature expanded
          freeVariables = surfaceTypeVariables expanded
          allVariables = nub (forallVariables <> [classInfoVar info] <> freeVariables)
          methodOnlyVariables = filter (/= classInfoVar info) allVariables
      if null methodOnlyVariables
        then pure ()
        else failKind (classMethodSpan method) "method-specific type variables are not supported yet"
      methodVariables <- mapM (makeVariable classVariable) allVariables
      let variableMap = methodVariables <> variables
      convertedPredicates <- mapM (convertPredicate variableMap) predicates
      if null convertedPredicates
        then pure ()
        else failKind (classMethodSpan method) "method-specific class constraints are not supported yet"
      (bodyKind, convertedBody) <- convertType variableMap body
      unifyKind (classMethodSpan method) bodyKind KStar
      let classPredicate = Pred (classMethodSpan method) (classInfoName info) (TVariable classVariable)
          scheme = Forall (map snd methodVariables) (classPredicate : convertedPredicates) convertedBody
      zonked <- zonkScheme scheme
      pure (classMethodName method, zonked)
    makeVariable classVariable name
      | name == classInfoVar info = pure (name, classVariable)
      | otherwise = do
          kind <- freshKindMeta
          uniqueId <- freshKindVariableId
          pure (name, freshTypeVariable uniqueId name kind)

checkInstanceDeclaration :: SynonymMap -> (InstanceInfo, Span) -> KindM InstanceDef
checkInstanceDeclaration synonyms (info, span_) = do
  state <- getKindState
  case lookup (instanceInfoClass info) (ksClasses state) of
    Nothing -> failKind span_ ("class not in scope in instance declaration: " <> instanceInfoClass info)
    Just _ -> pure ()
  let headNames = surfaceTypeVariables (instanceInfoHead info)
      contextNames = concatMap (surfaceTypeVariables . S.predType) (instanceInfoContext info)
      allNames = nub (headNames <> contextNames)
  variables <- mapM makeVariable allNames
  expandedHead <- expandSynonyms synonyms (instanceInfoHead info)
  (headKind, convertedHead) <- convertType variables expandedHead
  unifyKind span_ headKind KStar
  convertedContext <- mapM (convertPredicate variables) (instanceInfoContext info)
  case convertedHead of
    TVariable _ -> failKind span_ "instance head cannot be a bare type variable"
    _ -> pure ()
  let headVariables = ftvType convertedHead
      contextVariables = nub (concatMap ftvPred convertedContext)
      extraVariables = contextVariables \\ headVariables
  if null extraVariables
    then pure ()
    else failKind span_ "instance context mentions type variables that do not occur in the head"
  pure
    InstanceDef
      { instanceDefClass = instanceInfoClass info
      , instanceDefHead = convertedHead
      , instanceDefContext = convertedContext
      , instanceDefVars = map snd variables
      , instanceDefMethods = instanceInfoMethods info
      , instanceDefSpan = span_
      }
  where
    makeVariable name = do
      kind <- freshKindMeta
      uniqueId <- freshKindVariableId
      pure (name, freshTypeVariable uniqueId name kind)

checkSignatures :: SynonymMap -> RModule -> KindM [(ResolvedName, Scheme)]
checkSignatures synonyms rModule =
  mapM checkSignature
    [ (rFunName function, signature)
    | RFunDecl function <- rModuleDecls rModule
    , Just signature <- [rFunSignature function]
    ]
  where
    checkSignature (name, signature) = do
      expanded <- expandSynonyms synonyms signature
      scheme <- convertScheme [] expanded
      pure (name, scheme)

duplicateInstanceKey :: [InstanceDef] -> Maybe String
duplicateInstanceKey definitions = go [] keys
  where
    keys =
      [ ( show (instanceDefClass definition, normalizeType (instanceDefHead definition))
        , show (instanceDefClass definition) <> " " <> prettyType (normalizeType (instanceDefHead definition))
        )
      | definition <- definitions
      ]
    go _ [] = Nothing
    go seen ((key, label) : rest)
      | key `elem` seen = Just label
      | otherwise = go (key : seen) rest

zonkNamedScheme :: (ResolvedName, Scheme) -> KindM (ResolvedName, Scheme)
zonkNamedScheme (name, scheme) = do
  scheme' <- zonkScheme scheme
  pure (name, scheme')

zonkClassDef :: ClassDef -> KindM ClassDef
zonkClassDef definition = do
  variable <- zonkVariable (classDefVar definition)
  supers <- mapM zonkPredicate (classDefSupers definition)
  methods <- mapM zonkMethod (classDefMethods definition)
  pure definition {classDefVar = variable, classDefSupers = supers, classDefMethods = methods}
  where
    zonkVariable variable = do
      kind <- zonkKind (tvKind variable)
      pure variable {tvKind = kind}
    zonkMethod (name, scheme) = do
      scheme' <- zonkScheme scheme
      pure (name, scheme')

zonkInstanceDef :: InstanceDef -> KindM InstanceDef
zonkInstanceDef definition = do
  headType <- zonkType (instanceDefHead definition)
  context <- mapM zonkPredicate (instanceDefContext definition)
  variables <- mapM zonkVariable (instanceDefVars definition)
  pure definition {instanceDefHead = headType, instanceDefContext = context, instanceDefVars = variables}
  where
    zonkVariable variable = do
      kind <- zonkKind (tvKind variable)
      pure variable {tvKind = kind}

constructorScheme :: DataDef -> ConDef -> Scheme
constructorScheme definition constructor =
  Forall
    (dataDefVars definition)
    []
    (foldr TFun (conDefResult constructor) (conDefFields constructor))

-- Built-in data types -------------------------------------------------------

builtinDataDefs :: [DataDef]
builtinDataDefs =
  [ boolDataDef
  , listDataDef
  , maybeDataDef
  , eitherDataDef
  , unitDataDef
  , tuple2DataDef
  , tuple3DataDef
  , tuple4DataDef
  ]

primTyCon :: Name -> Kind -> Int -> TyCon
primTyCon name kind arity = TyCon name kind arity

boolDataDef :: DataDef
boolDataDef =
  DataDef
    { dataDefName = ResolvedName "Bool" (-2001)
    , dataDefTypeCon = primTyCon "Bool" KStar 0
    , dataDefVars = []
    , dataDefCons =
        [ ConDef (builtinName "False") [] (TCon (primTyCon "Bool" KStar 0))
        , ConDef (builtinName "True") [] (TCon (primTyCon "Bool" KStar 0))
        ]
    , dataDefDeriving = []
    }

listDataDef :: DataDef
listDataDef =
  let variable = freshTypeVariable (-2002) "a" KStar
      listType = TCon (primTyCon "[]" (KArrow KStar KStar) 1)
      appliedList = TApp listType (TVariable variable)
   in DataDef
        { dataDefName = ResolvedName "[]" (-2003)
        , dataDefTypeCon = primTyCon "[]" (KArrow KStar KStar) 1
        , dataDefVars = [variable]
        , dataDefCons =
            [ ConDef (builtinName "[]") [] appliedList
            , ConDef (builtinName ":") [TVariable variable, appliedList] appliedList
            ]
        , dataDefDeriving = []
        }

maybeDataDef :: DataDef
maybeDataDef =
  let variable = freshTypeVariable (-2004) "a" KStar
      maybeType = TCon (primTyCon "Maybe" (KArrow KStar KStar) 1)
      appliedMaybe = TApp maybeType (TVariable variable)
   in DataDef
        { dataDefName = ResolvedName "Maybe" (-2005)
        , dataDefTypeCon = primTyCon "Maybe" (KArrow KStar KStar) 1
        , dataDefVars = [variable]
        , dataDefCons =
            [ ConDef (builtinName "Nothing") [] appliedMaybe
            , ConDef (builtinName "Just") [TVariable variable] appliedMaybe
            ]
        , dataDefDeriving = []
        }

eitherDataDef :: DataDef
eitherDataDef =
  let leftVariable = freshTypeVariable (-2006) "a" KStar
      rightVariable = freshTypeVariable (-2007) "b" KStar
      eitherType = TCon (primTyCon "Either" (KArrow KStar (KArrow KStar KStar)) 2)
      appliedEither = TApp (TApp eitherType (TVariable leftVariable)) (TVariable rightVariable)
   in DataDef
        { dataDefName = ResolvedName "Either" (-2008)
        , dataDefTypeCon = primTyCon "Either" (KArrow KStar (KArrow KStar KStar)) 2
        , dataDefVars = [leftVariable, rightVariable]
        , dataDefCons =
            [ ConDef (builtinName "Left") [TVariable leftVariable] appliedEither
            , ConDef (builtinName "Right") [TVariable rightVariable] appliedEither
            ]
        , dataDefDeriving = []
        }

unitDataDef :: DataDef
unitDataDef =
  DataDef
    { dataDefName = ResolvedName "()" (-2009)
    , dataDefTypeCon = primTyCon "()" KStar 0
    , dataDefVars = []
    , dataDefCons = [ConDef (builtinName "()") [] (TCon (primTyCon "()" KStar 0))]
    , dataDefDeriving = []
    }

tuple2DataDef :: DataDef
tuple2DataDef =
  let firstVariable = freshTypeVariable (-2010) "a" KStar
      secondVariable = freshTypeVariable (-2011) "b" KStar
      tupleType = TCon (primTyCon "(,)" (KArrow KStar (KArrow KStar KStar)) 2)
      appliedTuple = TApp (TApp tupleType (TVariable firstVariable)) (TVariable secondVariable)
   in DataDef
        { dataDefName = ResolvedName "(,)" (-2012)
        , dataDefTypeCon = primTyCon "(,)" (KArrow KStar (KArrow KStar KStar)) 2
        , dataDefVars = [firstVariable, secondVariable]
        , dataDefCons = [ConDef (builtinName "(,)") [TVariable firstVariable, TVariable secondVariable] appliedTuple]
        , dataDefDeriving = []
        }

tuple3DataDef :: DataDef
tuple3DataDef =
  let firstVariable = freshTypeVariable (-2013) "a" KStar
      secondVariable = freshTypeVariable (-2014) "b" KStar
      thirdVariable = freshTypeVariable (-2015) "c" KStar
      tupleType = TCon (primTyCon "(,,)" (KArrow KStar (KArrow KStar (KArrow KStar KStar))) 3)
      appliedTuple =
        TApp
          (TApp (TApp tupleType (TVariable firstVariable)) (TVariable secondVariable))
          (TVariable thirdVariable)
   in DataDef
        { dataDefName = ResolvedName "(,,)" (-2016)
        , dataDefTypeCon = primTyCon "(,,)" (KArrow KStar (KArrow KStar (KArrow KStar KStar))) 3
        , dataDefVars = [firstVariable, secondVariable, thirdVariable]
        , dataDefCons =
            [ ConDef
                (builtinName "(,,)")
                [TVariable firstVariable, TVariable secondVariable, TVariable thirdVariable]
                appliedTuple
            ]
        , dataDefDeriving = []
        }

tuple4DataDef :: DataDef
tuple4DataDef =
  let firstVariable = freshTypeVariable (-2017) "a" KStar
      secondVariable = freshTypeVariable (-2018) "b" KStar
      thirdVariable = freshTypeVariable (-2019) "c" KStar
      fourthVariable = freshTypeVariable (-2020) "d" KStar
      tupleKind = KArrow KStar (KArrow KStar (KArrow KStar (KArrow KStar KStar)))
      tupleType = TCon (primTyCon "(,,,)" tupleKind 4)
      appliedTuple =
        TApp
          ( TApp
              (TApp (TApp tupleType (TVariable firstVariable)) (TVariable secondVariable))
              (TVariable thirdVariable)
          )
          (TVariable fourthVariable)
   in DataDef
        { dataDefName = ResolvedName "(,,,)" (-2021)
        , dataDefTypeCon = primTyCon "(,,,)" tupleKind 4
        , dataDefVars = [firstVariable, secondVariable, thirdVariable, fourthVariable]
        , dataDefCons =
            [ ConDef
                (builtinName "(,,,)")
                [ TVariable firstVariable
                , TVariable secondVariable
                , TVariable thirdVariable
                , TVariable fourthVariable
                ]
                appliedTuple
            ]
        , dataDefDeriving = []
        }
