-- | Hindley–Milner inference for ProtoHaskell.
--
-- The module intentionally follows the book's constraint-generation style:
-- inference emits equality constraints (unification) and class predicates,
-- then a separate generalization step decides which predicates belong to a
-- scheme.  Variable occurrences record evidence ids so that the elaborator
-- can later insert dictionary arguments.
module WYAH.Infer
  ( TypeError(..)
  , InferredModule(..)
  , InferredInstance(..)
  , InferredMethod(..)
  , inferModule
  , inferExpression
  , prettyInferredType
  ) where

import Data.List (nub)
import WYAH.Diagnostic
import WYAH.Kind
import WYAH.Prelude
import WYAH.Renamed
import WYAH.Span
import WYAH.Syntax (Literal(..), Name)
import qualified WYAH.Syntax as Surface
import WYAH.Type
import WYAH.Typed

data TypeError = TypeError
  { typeErrorSource :: FilePath
  , typeErrorSpan :: Span
  , typeErrorMessage :: String
  }
  deriving (Eq)

instance Show TypeError where
  show err =
    renderDiagnostic
      Diagnostic
        { diagnosticPhase = phaseName InferPhase
        , diagnosticSource = Just (typeErrorSource err)
        , diagnosticSpan = Just (typeErrorSpan err)
        , diagnosticMessage = typeErrorMessage err
        }

data InferredMethod = InferredMethod
  { inferredMethodName :: ResolvedName
  , inferredMethodScheme :: Scheme
  , inferredMethodMatches :: [TEMatch]
  }
  deriving (Eq, Show)

data InferredInstance = InferredInstance
  { inferredInstanceDef :: InstanceDef
  , inferredInstanceMethods :: [InferredMethod]
  }
  deriving (Eq, Show)

data InferredModule = InferredModule
  { inferredModule :: RModule
  , inferredDataDefs :: [DataDef]
  , inferredClassDefs :: [ClassDef]
  , inferredInstanceDefs :: [InstanceDef]
  , inferredBindings :: [TEBind]
  , inferredBindSchemes :: [(ResolvedName, Scheme)]
  , inferredInstances :: [InferredInstance]
  , inferredEvidence :: [(EvidenceId, Pred)]
  , inferredConstructorSchemes :: [(ResolvedName, Scheme)]
  }
  deriving (Eq, Show)

-- Inference monad -----------------------------------------------------------

data Constraint = Constraint
  { constraintSpan :: Span
  , constraintPredicate :: Pred
  , constraintEvidence :: EvidenceId
  }

data InferState = InferState
  { isSource :: FilePath
  , isNextType :: Int
  , isNextEvidence :: Int
  , isSubstitution :: [(TVar, Type)]
  , isConstraints :: [Constraint]
  , isEvidenceStore :: [(EvidenceId, Pred)]
  , isEnvironment :: [(ResolvedName, Scheme)]
  , isClassDefs :: [ClassDef]
  , isInstanceDefs :: [InstanceDef]
  , isGivens :: [Pred]
  , isTypeInfos :: [(Name, TypeInfo)]
  , isClassNames :: [Name]
  , isSynonyms :: SynonymMap
  }

newtype Infer a = Infer {runInfer :: InferState -> Either TypeError (a, InferState)}

instance Functor Infer where
  fmap f (Infer action) = Infer $ \state ->
    case action state of
      Left err -> Left err
      Right (value, state') -> Right (f value, state')

instance Applicative Infer where
  pure value = Infer $ \state -> Right (value, state)
  Infer pf <*> Infer pa = Infer $ \state ->
    case pf state of
      Left err -> Left err
      Right (f, state') ->
        case pa state' of
          Left err -> Left err
          Right (value, state'') -> Right (f value, state'')

instance Monad Infer where
  Infer action >>= f = Infer $ \state ->
    case action state of
      Left err -> Left err
      Right (value, state') -> runInfer (f value) state'

getInferState :: Infer InferState
getInferState = Infer $ \state -> Right (state, state)

modifyInferState :: (InferState -> InferState) -> Infer ()
modifyInferState f = Infer $ \state -> Right ((), f state)

failInfer :: Span -> String -> Infer a
failInfer span_ message = Infer $ \state ->
  Left
    TypeError
      { typeErrorSource = isSource state
      , typeErrorSpan = span_
      , typeErrorMessage = message
      }

freshType :: Kind -> Infer Type
freshType kind = do
  state <- getInferState
  let uniqueId = isNextType state
  modifyInferState $ \current -> current {isNextType = uniqueId + 1}
  pure (TVariable (freshTypeVariable uniqueId ("t" <> show uniqueId) kind))

freshEvidence :: Span -> Pred -> Infer EvidenceId
freshEvidence span_ predicate = do
  state <- getInferState
  let uniqueId = isNextEvidence state
      evidence = EvidenceId uniqueId
      constraint = Constraint span_ predicate evidence
  modifyInferState $ \current ->
    current
      { isNextEvidence = uniqueId + 1
      , isConstraints = isConstraints current <> [constraint]
      , isEvidenceStore = isEvidenceStore current <> [(evidence, predicate)]
      }
  pure evidence

currentSubstitution :: Infer [(TVar, Type)]
currentSubstitution = isSubstitution <$> getInferState

applyCurrent :: Type -> Infer Type
applyCurrent type_ = do
  substitution <- currentSubstitution
  pure (applyType substitution type_)

applyCurrentPredicate :: Pred -> Infer Pred
applyCurrentPredicate predicate = do
  substitution <- currentSubstitution
  pure (applyPred substitution predicate)

occursIn :: TVar -> Type -> Infer Bool
occursIn variable type_ = do
  type' <- applyCurrent type_
  pure (go type')
  where
    go (TVariable other) = variable == other
    go (TCon _) = False
    go (TApp function argument) = go function || go argument
    go (TFun argument result) = go argument || go result

unify :: Span -> Type -> Type -> Infer ()
unify span_ left right = do
  left' <- applyCurrent left
  right' <- applyCurrent right
  case (left', right') of
    (TVariable variable, TVariable other)
      | variable == other -> pure ()
    (TVariable variable, type_)
      | tvSkolem variable ->
          failInfer span_ ("cannot unify rigid type variable " <> tvName variable <> " with " <> prettyType type_)
      | otherwise -> bindVariable span_ variable type_
    (type_, TVariable variable)
      | tvSkolem variable ->
          failInfer span_ ("cannot unify " <> prettyType type_ <> " with rigid type variable " <> tvName variable)
      | otherwise -> bindVariable span_ variable type_
    (TCon leftConstructor, TCon rightConstructor)
      | tyConName leftConstructor == tyConName rightConstructor -> pure ()
    (TApp leftFunction leftArgument, TApp rightFunction rightArgument) -> do
      unify span_ leftFunction rightFunction
      unify span_ leftArgument rightArgument
    (TFun leftArgument leftResult, TFun rightArgument rightResult) -> do
      unify span_ leftArgument rightArgument
      unify span_ leftResult rightResult
    _ ->
      failInfer
        span_
        ("cannot unify " <> prettyType left' <> " with " <> prettyType right')

bindVariable :: Span -> TVar -> Type -> Infer ()
bindVariable span_ variable type_ = do
  occurs <- occursIn variable type_
  if occurs
    then failInfer span_ ("occurs check failed: " <> tvName variable <> " occurs in " <> prettyType type_)
    else do
      state <- getInferState
      modifyInferState $ \current ->
        current {isSubstitution = (variable, type_) : isSubstitution current}

instanceType :: Scheme -> Infer (Type, [Pred])
instanceType scheme = instantiate scheme

instantiate :: Scheme -> Infer (Type, [Pred])
instantiate (Forall variables predicates body) = do
  replacements <- mapM (freshType . tvKind) variables
  let substitution = zip variables replacements
  pure (applyType substitution body, applyPredicates substitution predicates)

-- Public inference entry points --------------------------------------------

inferModule :: FilePath -> CheckedModule -> Either TypeError InferredModule
inferModule file checked = do
  let initial =
        InferState
          { isSource = file
          , isNextType = 100
          , isNextEvidence = 0
          , isSubstitution = []
          , isConstraints = []
          , isEvidenceStore = []
          , isEnvironment = initialEnvironment checked
          , isClassDefs = checkedClassDefs checked
          , isInstanceDefs = checkedInstanceDefs checked
          , isGivens = []
          , isTypeInfos = checkedTypeInfos checked
          , isClassNames = map classDefName (checkedClassDefs checked)
          , isSynonyms = checkedSynonyms checked
          }
  (result, _) <- runInfer (inferModuleM checked) initial
  pure result

inferExpression :: FilePath -> CheckedModule -> RExpr -> Either TypeError (Scheme, TExpr, [(EvidenceId, Pred)])
inferExpression file checked expression = do
  let initial =
        InferState
          { isSource = file
          , isNextType = 100
          , isNextEvidence = 0
          , isSubstitution = []
          , isConstraints = []
          , isEvidenceStore = []
          , isEnvironment = initialEnvironment checked
          , isClassDefs = checkedClassDefs checked
          , isInstanceDefs = checkedInstanceDefs checked
          , isGivens = []
          , isTypeInfos = checkedTypeInfos checked
          , isClassNames = map classDefName (checkedClassDefs checked)
          , isSynonyms = checkedSynonyms checked
          }
  ((scheme, typed, evidence), _) <- runInfer (inferRepl checked expression) initial
  pure (scheme, typed, evidence)

prettyInferredType :: Scheme -> String
prettyInferredType = show

-- Environment ---------------------------------------------------------------

initialEnvironment :: CheckedModule -> [(ResolvedName, Scheme)]
initialEnvironment checked =
  preludeSchemes
    <> checkedConstructorSchemes checked
    <> classMethodSchemes (checkedClassDefs checked)
    <> checkedSignatures checked
  where
    classMethodSchemes definitions =
      concat [methods | definition <- definitions, let methods = classDefMethods definition]

lookupScheme :: Span -> ResolvedName -> Infer Scheme
lookupScheme span_ name = do
  state <- getInferState
  case lookup name (isEnvironment state) of
    Just scheme_ -> pure scheme_
    Nothing -> failInfer span_ ("not in scope during inference: " <> resolvedText name)

extendEnvironment :: [(ResolvedName, Scheme)] -> Infer a -> Infer a
extendEnvironment bindings action = do
  state <- getInferState
  modifyInferState $ \current -> current {isEnvironment = bindings <> isEnvironment current}
  result <- action
  modifyInferState $ \current -> current {isEnvironment = isEnvironment state}
  pure result

withGivens :: [Pred] -> Infer a -> Infer a
withGivens predicates action = do
  state <- getInferState
  modifyInferState $ \current -> current {isGivens = predicates <> isGivens current}
  result <- action
  modifyInferState $ \current -> current {isGivens = isGivens state}
  pure result

-- Module inference ----------------------------------------------------------

inferModuleM :: CheckedModule -> Infer InferredModule
inferModuleM checked = do
  let functions =
        [ function
        | RFunDecl function <- rModuleDecls (checkedModule checked)
        ]
      signatureSchemes = checkedSignatures checked
  assumptions <- mapM (makeAssumption signatureSchemes) functions
  results <- extendEnvironment assumptions (inferFunctionsInOrder signatureSchemes functions)
  do
    instances <- mapM (inferInstanceM checked) (checkedInstanceDefs checked)
    finalState <- getInferState
    let finalSubstitution = isSubstitution finalState
        bindings = [bind | (bind, _, _) <- results]
        schemes = [(tBindName bind, scheme) | (bind, scheme, _) <- results]
        evidence = [(evidenceId, applyPred finalSubstitution predicate) | (evidenceId, predicate) <- isEvidenceStore finalState]
    pure
      InferredModule
        { inferredModule = checkedModule checked
        , inferredDataDefs = checkedDataDefs checked
        , inferredClassDefs = checkedClassDefs checked
        , inferredInstanceDefs = checkedInstanceDefs checked
        , inferredBindings = bindings
        , inferredBindSchemes = schemes
        , inferredInstances = instances
        , inferredEvidence = evidence
        , inferredConstructorSchemes = checkedConstructorSchemes checked
        }
  where
    makeAssumption signatureSchemes function =
      case lookup (rFunName function) signatureSchemes of
        Just scheme_ -> pure (rFunName function, scheme_)
        Nothing -> do
          typeVariable <- freshType KStar
          pure (rFunName function, Forall [] [] typeVariable)

inferFunctionsInOrder ::
  [(ResolvedName, Scheme)] ->
  [RFun] ->
  Infer [(TEBind, Scheme, [(TVar, Type)])]
inferFunctionsInOrder signatureSchemes functions = go [] functions []
  where
    topLevelNames = map rFunName functions
    dependencies = [(rFunName function, dependenciesOf function topLevelNames) | function <- functions]
    go _ [] accumulated = pure (reverse accumulated)
    go processed pending accumulated =
      let ready = [function | function <- pending, readyFor processed function]
          group = if null ready then pending else ready
          remaining = [function | function <- pending, function `notElem` group]
       in do
            results <- mapM (inferTopLevelFunction signatureSchemes) group
            go (processed <> map rFunName group) remaining (reverse results <> accumulated)

    readyFor processed function =
      case lookup (rFunName function) dependencies of
        Nothing -> True
        Just names -> all (\name -> name == rFunName function || name `elem` processed) names

dependenciesOf :: RFun -> [ResolvedName] -> [ResolvedName]
dependenciesOf function topLevelNames =
  [name | name <- nub (concatMap matchVariables (rFunMatches function)), name `elem` topLevelNames]

matchVariables :: RMatch -> [ResolvedName]
matchVariables match =
  concatMap guardVariables (rhsGuards (rMatchRhs match))
    <> rhsVariables (rMatchRhs match)
    <> concatMap declarationVariables (rMatchWhere match)
  where
    rhsVariables (RPlainRhs _ expression) = expressionVariables expression
    rhsVariables (RGuardedRhs _ clauses) = concatMap (expressionVariables . snd) clauses
    rhsGuards (RPlainRhs _ _) = []
    rhsGuards (RGuardedRhs _ clauses) = map fst clauses
    guardVariables (RGuardExpr _ expression) = expressionVariables expression
    guardVariables (RGuardPattern _ _ expression) = expressionVariables expression
    guardVariables (RGuardList _ guards) = concatMap guardVariables guards

declarationVariables :: RDecl -> [ResolvedName]
declarationVariables declaration = case declaration of
  RFunDecl function -> concatMap matchVariables (rFunMatches function)
  RInstanceDecl info -> concatMap (concatMap matchVariables . snd) (instanceInfoMethods info)
  _ -> []

expressionVariables :: RExpr -> [ResolvedName]
expressionVariables expression = case expression of
  RVar _ name -> [name]
  RCon _ _ -> []
  RLit _ _ -> []
  RApp _ function argument -> expressionVariables function <> expressionVariables argument
  RLam _ _ body -> expressionVariables body
  RLet _ bindings body -> concatMap bindVariables bindings <> expressionVariables body
  RIf _ condition trueBranch falseBranch ->
    expressionVariables condition <> expressionVariables trueBranch <> expressionVariables falseBranch
  RCase _ scrutinee alternatives ->
    expressionVariables scrutinee <> concatMap alternativeVariables alternatives
  RAnn _ inner _ -> expressionVariables inner
  RDo _ statements -> concatMap statementVariables statements
  RComp _ result statements -> expressionVariables result <> concatMap statementVariables statements

bindVariables :: RBind -> [ResolvedName]
bindVariables bind = concatMap matchVariables (rBindMatches bind) <> concatMap declarationVariables (rMatchWhere (head (rBindMatches bind)))

alternativeVariables :: RAlt -> [ResolvedName]
alternativeVariables alternative =
  concatMap guardVariablesAlt (rhsGuardsAlt (rAltRhs alternative))
    <> rhsVariablesAlt (rAltRhs alternative)
    <> concatMap declarationVariables (rAltWhere alternative)
  where
    rhsVariablesAlt (RPlainRhs _ expression) = expressionVariables expression
    rhsVariablesAlt (RGuardedRhs _ clauses) = concatMap (expressionVariables . snd) clauses
    rhsGuardsAlt (RPlainRhs _ _) = []
    rhsGuardsAlt (RGuardedRhs _ clauses) = map fst clauses
    guardVariablesAlt (RGuardExpr _ expression) = expressionVariables expression
    guardVariablesAlt (RGuardPattern _ _ expression) = expressionVariables expression
    guardVariablesAlt (RGuardList _ guards) = concatMap guardVariablesAlt guards

statementVariables :: RStmt -> [ResolvedName]
statementVariables statement = case statement of
  RGenerator _ _ expression -> expressionVariables expression
  RQualifier _ expression -> expressionVariables expression
  RLetStmt _ bindings -> concatMap bindVariables bindings

inferTopLevelFunction ::
  [(ResolvedName, Scheme)] ->
  RFun ->
  Infer (TEBind, Scheme, [(TVar, Type)])
inferTopLevelFunction signatureSchemes function = do
  let name = rFunName function
  assumption <-
    case lookup name signatureSchemes of
      Just scheme_ -> pure scheme_
      Nothing -> do
        state <- getInferState
        case lookup name (isEnvironment state) of
          Just scheme_ -> pure scheme_
          Nothing -> failInfer (rFunSpan function) "internal inference error: missing function assumption"
  mark <- length . isConstraints <$> getInferState
  let unpackScheme (Forall variables predicates body) = (variables, predicates, body)
  case lookup name signatureSchemes of
    Just declaredScheme -> do
      let (variables, predicates, body) = unpackScheme declaredScheme
      skolems <- mapM (freshSkolem . tvKind) variables
      let substitution = zip variables skolems
          expectedType = applyType substitution body
          expectedPredicates = applyPredicates substitution predicates
      (matches, inferredType, _) <- withGivens expectedPredicates (inferFunctionMatches function)
      unify (rFunSpan function) inferredType expectedType
      withGivens expectedPredicates (solveRemainingConstraints mark (rFunSpan function) expectedPredicates)
      setEnvironmentScheme name declaredScheme
      current <- currentSubstitution
      let skolemVariables = [variable | TVariable variable <- skolems]
          evidenceScheme = Forall skolemVariables expectedPredicates expectedType
      pure (TEBind (rFunSpan function) name evidenceScheme matches, declaredScheme, current)
    Nothing -> do
      let assumptionType = schemeBody assumption
      (matches, inferredType, _) <- inferFunctionMatches function
      unify (rFunSpan function) inferredType assumptionType
      outerState <- getInferState
      let outerEnvironment =
            [ (n, s) | (n, s) <- isEnvironment outerState, n /= name ]
      (scheme_, substitutions) <- generalizeBinding outerEnvironment (rFunSpan function) inferredType mark
      setEnvironmentScheme name scheme_
      pure (TEBind (rFunSpan function) name scheme_ matches, scheme_, substitutions)
  where
    schemeBody (Forall _ _ body) = body

setEnvironmentScheme :: ResolvedName -> Scheme -> Infer ()
setEnvironmentScheme name scheme_ = do
  state <- getInferState
  modifyInferState $ \current ->
    current {isEnvironment = (name, scheme_) : filter ((/= name) . fst) (isEnvironment current)}

freshSkolem :: Kind -> Infer Type
freshSkolem kind = do
  state <- getInferState
  let uniqueId = isNextType state
  modifyInferState $ \current -> current {isNextType = uniqueId + 1}
  pure (TVariable ((freshTypeVariable uniqueId ("s" <> show uniqueId) kind) {tvSkolem = True}))

inferFunctionMatches :: RFun -> Infer ([TEMatch], Type, [(TVar, Type)])
inferFunctionMatches function = do
  results <- mapM inferMatch (rFunMatches function)
  let types = [type_ | (_, type_) <- results]
  case types of
    [] -> failInfer (rFunSpan function) "function declaration has no equations"
    first : rest -> do
      mapM_ (unify (rFunSpan function) first) rest
  substitution <- currentSubstitution
  pure (map fst results, applyType substitution (head types), substitution)

inferMatch :: RMatch -> Infer (TEMatch, Type)
inferMatch match = do
  (patternBindings, argumentTypes) <- inferMatchPatterns (rMatchPatterns match)
  let patternEnvironment = [(name, Forall [] [] type_) | (name, type_) <- patternBindings]
  (whereBindings, whereSchemes) <-
    locallyBindings patternEnvironment (inferLocalDecls (rMatchWhere match))
  (rhs, resultType) <-
    locallyBindings (whereSchemes <> patternEnvironment) (inferRhs (rMatchRhs match))
  let functionType = foldr TFun resultType argumentTypes
  pure (TEMatch (rMatchSpan match) (rMatchPatterns match) rhs whereBindings, functionType)

-- Pattern inference ---------------------------------------------------------

inferMatchPatterns :: [RPat] -> Infer ([(ResolvedName, Type)], [Type])
inferMatchPatterns patterns = do
  results <- mapM inferArgumentPattern patterns
  pure (concatMap fst results, map snd results)
  where
    inferArgumentPattern pattern_ = do
      argumentType <- freshType KStar
      bindings <- inferPattern pattern_ argumentType
      pure (bindings, argumentType)

inferPattern :: RPat -> Type -> Infer [(ResolvedName, Type)]
inferPattern pattern_ expectedType = case pattern_ of
  RPVar _ name -> pure [(name, expectedType)]
  RPWild _ -> pure []
  RPLit span_ literal -> do
    literalType <- literalTypeOf span_ literal
    unify span_ expectedType literalType
    pure []
  RPCon span_ name arguments -> do
    scheme_ <- lookupScheme span_ name
    (constructorType, predicates) <- instantiate scheme_
    if not (null predicates)
      then failInfer span_ "constructor has class constraints"
      else pure ()
    let (argumentTypes, resultType) = splitFunctionType constructorType
    if length argumentTypes /= length arguments
      then
        failInfer
          span_
          ( "constructor "
              <> resolvedText name
              <> " expects "
              <> show (length argumentTypes)
              <> " arguments, but "
              <> show (length arguments)
              <> " were supplied"
          )
      else do
        unify span_ resultType expectedType
        bindings <- mapM (uncurry inferPattern) (zip arguments argumentTypes)
        pure (concat bindings)
  RPAs _ name inner -> do
    bindings <- inferPattern inner expectedType
    pure ((name, expectedType) : bindings)
  RPAnn span_ inner annotation -> do
    annotationType <- convertAnnotationType span_ annotation
    unify span_ expectedType annotationType
    inferPattern inner annotationType

splitFunctionType :: Type -> ([Type], Type)
splitFunctionType (TFun argument result) =
  let (arguments, finalResult) = splitFunctionType result
   in (argument : arguments, finalResult)
splitFunctionType type_ = ([], type_)

convertAnnotationType :: Span -> Surface.Type -> Infer Type
convertAnnotationType span_ type_ = do
  state <- getInferState
  expanded <-
    case expandSurfaceSynonyms (isSynonyms state) type_ of
      Left err -> failInfer (kindErrorSpan err) (kindErrorMessage err)
      Right result -> pure result
  let variableNames = nub (annotationVariables expanded)
  variables <- mapM (\name -> do variableType <- freshType KStar; pure (name, variableType)) variableNames
  convert variables expanded
  where
    convert variables type__ = case type__ of
      Surface.TyCon typeSpan name
        | name == "String" -> pure (TApp (TCon (TyCon "[]" (KArrow KStar KStar) 1)) (TCon (TyCon "Char" KStar 0)))
        | otherwise -> do
            maybeInfo <- typeInfo name
            case maybeInfo of
              Just info -> pure (TCon (TyCon name (typeInfoKind info) (typeInfoArity info)))
              Nothing -> failInfer typeSpan ("type constructor not in scope: " <> name)
      Surface.TyVar typeSpan name ->
        case lookup name variables of
          Just variable -> pure variable
          Nothing -> failInfer typeSpan ("type variable not in scope: " <> name)
      Surface.TyApp typeSpan function argument -> do
        functionType <- convert variables function
        argumentType <- convert variables argument
        pure (TApp functionType argumentType)
      Surface.TyFun _ argument result -> do
        argumentType <- convert variables argument
        resultType <- convert variables result
        pure (TFun argumentType resultType)
      Surface.TyList typeSpan element -> do
        elementType <- convert variables element
        pure (TApp (TCon (TyCon "[]" (KArrow KStar KStar) 1)) elementType)
      Surface.TyTuple typeSpan elements -> do
        converted <- mapM (convert variables) elements
        let tupleName = "(" <> replicate (length elements - 1) ',' <> ")"
        maybeInfo <- typeInfo tupleName
        case maybeInfo of
          Just info -> pure (foldl TApp (TCon (TyCon tupleName (typeInfoKind info) (typeInfoArity info))) converted)
          Nothing -> failInfer typeSpan ("unsupported tuple type of size " <> show (length elements))
      Surface.TyForall typeSpan _ body -> do
        bodyType <- convert variables body
        pure bodyType
      Surface.TyContext typeSpan _ _ ->
        failInfer typeSpan "class contexts in expression annotations are not supported yet"
    typeInfo name = do
      current <- getInferState
      pure (lookup name (isTypeInfos current))
    annotationVariables type__ = case type__ of
      Surface.TyVar _ name -> [name]
      Surface.TyCon _ _ -> []
      Surface.TyApp _ function argument -> annotationVariables function <> annotationVariables argument
      Surface.TyFun _ argument result -> annotationVariables argument <> annotationVariables result
      Surface.TyList _ element -> annotationVariables element
      Surface.TyTuple _ elements -> concatMap annotationVariables elements
      Surface.TyForall _ variables body -> filter (`notElem` variables) (annotationVariables body)
      Surface.TyContext _ _ body -> annotationVariables body

literalTypeOf :: Span -> Literal -> Infer Type
literalTypeOf _ literal = case literal of
  LInt _ -> pure (TCon (TyCon "Int" KStar 0))
  LBool _ -> pure (TCon (TyCon "Bool" KStar 0))
  LChar _ -> pure (TCon (TyCon "Char" KStar 0))
  LString _ -> pure (TApp (TCon (TyCon "[]" (KArrow KStar KStar) 1)) (TCon (TyCon "Char" KStar 0)))

-- Expression inference ------------------------------------------------------

inferExpr :: RExpr -> Infer (TExpr, Type)
inferExpr expression = case expression of
  RVar span_ name -> do
    scheme_ <- lookupScheme span_ name
    (type_, predicates) <- instantiate scheme_
    evidence <- mapM (freshEvidence span_) predicates
    pure (TEVar span_ name type_ evidence, type_)
  RCon span_ name -> do
    scheme_ <- lookupScheme span_ name
    (type_, predicates) <- instantiate scheme_
    if null predicates
      then pure (TECon span_ name type_, type_)
      else failInfer span_ "constructor has class constraints"
  RLit span_ literal -> do
    type_ <- literalTypeOf span_ literal
    pure (TELit span_ literal type_, type_)
  RApp span_ function argument -> do
    (typedFunction, functionType) <- inferExpr function
    (typedArgument, argumentType) <- inferExpr argument
    resultType <- freshType KStar
    unify span_ functionType (TFun argumentType resultType)
    pure (TEApp span_ typedFunction typedArgument resultType, resultType)
  RLam span_ names body -> inferLambda span_ names body
  RLet span_ binds body -> do
    (typedBindings, schemes) <- inferLocalBinds binds
    (typedBody, bodyType) <- locallyBindings schemes (inferExpr body)
    pure (TELet span_ typedBindings typedBody bodyType, bodyType)
  RIf span_ condition trueBranch falseBranch -> do
    (typedCondition, conditionType) <- inferExpr condition
    unify span_ conditionType (TCon (TyCon "Bool" KStar 0))
    (typedTrue, trueType) <- inferExpr trueBranch
    (typedFalse, falseType) <- inferExpr falseBranch
    unify span_ trueType falseType
    pure (TEIf span_ typedCondition typedTrue typedFalse trueType, trueType)
  RCase span_ scrutinee alternatives -> do
    (typedScrutinee, scrutineeType) <- inferExpr scrutinee
    typedAlternatives <- mapM (inferAlternative scrutineeType) alternatives
    case [tAltRhs alternative | alternative <- typedAlternatives] of
      [] -> failInfer span_ "case expression has no alternatives"
      first : rest -> do
        firstType <- rhsType first
        mapM_ (\rhs -> rhsType rhs >>= unify span_ firstType) rest
        pure (TECase span_ typedScrutinee typedAlternatives firstType, firstType)
  RAnn span_ inner annotation -> do
    (typed, type_) <- inferExpr inner
    annotationType <- convertAnnotationType span_ annotation
    unify span_ type_ annotationType
    pure (typed, type_)
  RDo span_ _ -> failInfer span_ "do-notation must be desugared before inference"
  RComp span_ _ _ -> failInfer span_ "list comprehensions must be desugared before inference"

inferLambda :: Span -> [ResolvedName] -> RExpr -> Infer (TExpr, Type)
inferLambda span_ names body = case names of
  [] -> do
    (typedBody, bodyType) <- inferExpr body
    pure (typedBody, bodyType)
  name : rest -> do
    argumentType <- freshType KStar
    (typedBody, bodyType) <-
      locallyBindings [(name, Forall [] [] argumentType)] (inferLambda span_ rest body)
    pure (TELam span_ name argumentType typedBody (TFun argumentType bodyType), TFun argumentType bodyType)

inferAlternative :: Type -> RAlt -> Infer TEAlt
inferAlternative scrutineeType alternative = do
  patternBindings <- inferPattern (rAltPattern alternative) scrutineeType
  let patternEnvironment = [(name, Forall [] [] type_) | (name, type_) <- patternBindings]
  (whereBindings, whereSchemes) <-
    locallyBindings patternEnvironment (inferLocalDecls (rAltWhere alternative))
  (rhs, _) <-
    locallyBindings (whereSchemes <> patternEnvironment) (inferRhs (rAltRhs alternative))
  pure (TEAlt (rAltSpan alternative) (rAltPattern alternative) rhs whereBindings)

rhsType :: TRhs -> Infer Type
rhsType rhs = case rhs of
  TRPlain expression -> pure (tExprOf expression)
  TRGuarded [] -> failInfer NoSpan "empty guarded right-hand side"
  TRGuarded ((_, expression) : _) -> pure (tExprOf expression)

inferRhs :: RRhs -> Infer (TRhs, Type)
inferRhs rhs = case rhs of
  RPlainRhs _ expression -> do
    (typed, type_) <- inferExpr expression
    pure (TRPlain typed, type_)
  RGuardedRhs _ clauses -> do
    results <- mapM inferClause clauses
    let types = [type_ | (_, type_) <- results]
    case types of
      [] -> failInfer NoSpan "guarded equation has no guards"
      first : rest -> mapM_ (unify NoSpan first) rest
    pure (TRGuarded (map fst results), head types)
  where
    inferClause (guard_, expression) = do
      (typedGuard, bindings) <- inferGuard guard_
      let bindingSchemes = [(name, Forall [] [] type_) | (name, type_) <- bindings]
      (typedExpression, type_) <-
        locallyBindings bindingSchemes (inferExpr expression)
      pure ((typedGuard, typedExpression), type_)

inferGuard :: RGuard -> Infer (TRGuard, [(ResolvedName, Type)])
inferGuard guard_ = case guard_ of
  RGuardExpr span_ expression -> do
    (typed, type_) <- inferExpr expression
    unify span_ type_ (TCon (TyCon "Bool" KStar 0))
    pure (TRGuardExpr typed, [])
  RGuardPattern span_ pattern_ expression -> do
    (typedExpression, expressionType) <- inferExpr expression
    bindings <- inferPattern pattern_ expressionType
    pure (TRGuardPattern pattern_ typedExpression, bindings)
  RGuardList _ guards -> do
    (renamed, bindings) <- go [] guards
    pure (TRGuardList renamed, bindings)
    where
      go _ [] = pure ([], [])
      go scope (guard_ : rest) = do
        let scopeSchemes = [(name, Forall [] [] type_) | (name, type_) <- scope]
        (renamedGuard, bindings) <- locallyBindings scopeSchemes (inferGuard guard_)
        (renamedRest, restBindings) <- go (bindings <> scope) rest
        pure (renamedGuard : renamedRest, bindings <> restBindings)

-- Local binding groups ------------------------------------------------------

inferLocalDecls :: [RDecl] -> Infer ([TEBind], [(ResolvedName, Scheme)])
inferLocalDecls declarations =
  inferLocalBinds [bind | RFunDecl function <- declarations, Just bind <- [functionToBind function]]

inferLocalBinds :: [RBind] -> Infer ([TEBind], [(ResolvedName, Scheme)])
inferLocalBinds binds = do
  assumptions <- mapM makeAssumption binds
  extendEnvironment assumptions $ do
    results <- mapM (inferOne (map fst assumptions)) binds
    let typed = [typedBind | (typedBind, _) <- results]
        schemes = [(tBindName typedBind, scheme_) | (typedBind, scheme_) <- results]
    pure (typed, schemes)
  where
    makeAssumption bind =
      case rBindSignature bind of
        Just signature -> do
          annotationType <- convertAnnotationType (rBindSpan bind) signature
          pure (rBindName bind, Forall [] [] annotationType)
        Nothing -> do
          typeVariable <- freshType KStar
          pure (rBindName bind, Forall [] [] typeVariable)
    inferOne groupNames bind = do
      mark <- length . isConstraints <$> getInferState
      state <- getInferState
      let assumption =
            case lookup (rBindName bind) (isEnvironment state) of
              Just scheme_ -> scheme_
              Nothing -> Forall [] [] (TCon (TyCon "Int" KStar 0))
          assumptionType = schemeBody assumption
          outerEnvironment =
            [ (n, scheme_) | (n, scheme_) <- isEnvironment state, n `notElem` groupNames ]
      (matches, inferredType, _) <- inferBindMatches bind
      unify (rBindSpan bind) inferredType assumptionType
      (scheme_, _) <- generalizeBinding outerEnvironment (rBindSpan bind) inferredType mark
      pure (TEBind (rBindSpan bind) (rBindName bind) scheme_ matches, scheme_)
    schemeBody (Forall _ _ body) = body

inferBindMatches :: RBind -> Infer ([TEMatch], Type, [(TVar, Type)])
inferBindMatches bind = do
  results <- mapM inferMatch (rBindMatches bind)
  let types = [type_ | (_, type_) <- results]
  case types of
    [] -> failInfer (rBindSpan bind) "local binding has no equations"
    first : rest -> mapM_ (unify (rBindSpan bind) first) rest
  substitution <- currentSubstitution
  pure (map fst results, applyType substitution (head types), substitution)

tExprOf :: TExpr -> Type
tExprOf expression = case expression of
  TEVar _ _ type_ _ -> type_
  TECon _ _ type_ -> type_
  TELit _ _ type_ -> type_
  TEApp _ _ _ type_ -> type_
  TELam _ _ _ _ type_ -> type_
  TELet _ _ _ type_ -> type_
  TEIf _ _ _ _ type_ -> type_
  TECase _ _ _ type_ -> type_

functionToBind :: RFun -> Maybe RBind
functionToBind function =
  Just
    RBind
      { rBindSpan = rFunSpan function
      , rBindName = rFunName function
      , rBindMatches = rFunMatches function
      , rBindSignature = rFunSignature function
      }

locallyBindings :: [(ResolvedName, Scheme)] -> Infer a -> Infer a
locallyBindings = extendEnvironment

-- Generalization and constraint solving ------------------------------------

generalizeBinding ::
  [(ResolvedName, Scheme)] ->
  Span ->
  Type ->
  Int ->
  Infer (Scheme, [(TVar, Type)])
generalizeBinding outerEnvironment span_ type_ mark = do
  substitution <- currentSubstitution
  constraints <- isConstraints <$> getInferState
  let newConstraints = drop mark constraints
      type' = applyType substitution type_
      environmentVariables = nub (concatMap (ftvScheme . snd) outerEnvironment)
      quantified = nub (ftvType type' ++ [])
      relevantVariables = filter (`notElem` environmentVariables) quantified
      relevantPredicates =
        nub
          [ applyPred substitution (constraintPredicate constraint)
          | constraint <- newConstraints
          , let predicate = applyPred substitution (constraintPredicate constraint)
          , let predicateVariables = ftvPred predicate
          , not (null predicateVariables)
          , all (`elem` relevantVariables) predicateVariables
          ]
      irrelevantConstraints =
        [ (constraintSpan constraint, applyPred substitution (constraintPredicate constraint))
        | constraint <- newConstraints
        , let predicate = applyPred substitution (constraintPredicate constraint)
        , let predicateVariables = ftvPred predicate
        , null predicateVariables || not (all (`elem` relevantVariables) predicateVariables)
        ]
  mapM_ (uncurry solvePredicate) irrelevantConstraints
  removeConstraintsFrom mark
  pure (Forall relevantVariables relevantPredicates type', substitution)

removeConstraintsFrom :: Int -> Infer ()
removeConstraintsFrom mark = do
  state <- getInferState
  modifyInferState $ \current -> current {isConstraints = take mark (isConstraints current)}

solveRemainingConstraints :: Int -> Span -> [Pred] -> Infer ()
solveRemainingConstraints mark span_ predicates = do
  substitution <- currentSubstitution
  constraints <- isConstraints <$> getInferState
  let newConstraints =
        [ applyPred substitution (constraintPredicate constraint)
        | constraint <- drop mark constraints
        ]
  mapM_ (solvePredicate span_) newConstraints
  removeConstraintsFrom mark

solvePredicate :: Span -> Pred -> Infer ()
solvePredicate span_ predicate = do
  state <- getInferState
  substitution <- currentSubstitution
  let predicate' = applyPred substitution predicate
  entailedByGivens predicate' (isGivens state) >>= \isEntailed ->
    if isEntailed
    then pure ()
    else do
      solved <- solveByInstance 0 predicate'
      if solved
        then pure ()
        else
          failInfer
            span_
            ("cannot deduce " <> prettyPred predicate')

entailedByGivens :: Pred -> [Pred] -> Infer Bool
entailedByGivens wanted givens = go givens
  where
    go [] = pure False
    go (given : rest)
      | wanted == given = pure True
      | otherwise = do
          supers <- superclassPredicatesOf given
          case [predicate | predicate <- supers, predicate == wanted] of
            _ : _ -> pure True
            [] -> go rest

superclassPredicatesOf :: Pred -> Infer [Pred]
superclassPredicatesOf predicate = do
  state <- getInferState
  pure (closure [] predicate (isClassDefs state))
  where
    closure visited current classes
      | current `elem` visited = []
      | otherwise =
          case lookup (predClass current) [(classDefName definition, definition) | definition <- classes] of
            Nothing -> []
            Just definition ->
              let supers =
                    [ substituteClassVariable (classDefVar definition) (predType current) super
                    | super <- classDefSupers definition
                    ]
               in supers <> concatMap (\super -> closure (current : visited) super classes) supers

substituteClassVariable :: TVar -> Type -> Pred -> Pred
substituteClassVariable variable replacement predicate =
  predicate {predType = substituteType variable replacement (predType predicate)}

substituteType :: TVar -> Type -> Type -> Type
substituteType variable replacement type_ = case type_ of
  TVariable other | other == variable -> replacement
  TVariable other -> TVariable other
  TCon constructor -> TCon constructor
  TApp function argument -> TApp (substituteType variable replacement function) (substituteType variable replacement argument)
  TFun argument result -> TFun (substituteType variable replacement argument) (substituteType variable replacement result)

solveByInstance :: Int -> Pred -> Infer Bool
solveByInstance depth predicate
  | depth > 20 = pure False
  | otherwise = do
      state <- getInferState
      let candidates =
            [ instance_
            | instance_ <- isInstanceDefs state
            , instanceDefClass instance_ == predClass predicate
            ]
      tryInstances depth predicate candidates

isBareInstanceVariable :: Type -> Bool
isBareInstanceVariable (TVariable _) = True
isBareInstanceVariable _ = False

tryInstances :: Int -> Pred -> [InstanceDef] -> Infer Bool
tryInstances _ _ [] = pure False
tryInstances depth predicate (instance_ : rest)
  | isBareInstanceVariable (instanceDefHead instance_) = tryInstances depth predicate rest
  | otherwise =
      case matchInstance instance_ (predType predicate) of
        Nothing -> tryInstances depth predicate rest
        Just substitutions -> do
          let contextPredicates = map (applyPred substitutions) (instanceDefContext instance_)
          results <- mapM (solveByInstance (depth + 1)) contextPredicates
          if and results
            then pure True
            else tryInstances depth predicate rest

matchInstance :: InstanceDef -> Type -> Maybe [(TVar, Type)]
matchInstance instance_ predicateType =
  matchType (instanceDefHead instance_) predicateType []
  where
    matchType (TVariable variable) type_ substitutions = case lookup variable substitutions of
      Nothing -> Just ((variable, type_) : substitutions)
      Just existing -> if existing == type_ then Just substitutions else Nothing
    matchType (TCon left) (TCon right) substitutions
      | tyConName left == tyConName right = Just substitutions
      | otherwise = Nothing
    matchType (TApp leftFunction leftArgument) (TApp rightFunction rightArgument) substitutions = do
      substitutions1 <- matchType leftFunction rightFunction substitutions
      matchType leftArgument rightArgument substitutions1
    matchType (TFun leftArgument leftResult) (TFun rightArgument rightResult) substitutions = do
      substitutions1 <- matchType leftArgument rightArgument substitutions
      matchType leftResult rightResult substitutions1
    matchType _ _ _ = Nothing

-- Instances -----------------------------------------------------------------

inferInstanceM :: CheckedModule -> InstanceDef -> Infer InferredInstance
inferInstanceM checked instance_ = do
  let classDefinition =
        case [definition | definition <- checkedClassDefs checked, classDefName definition == instanceDefClass instance_] of
          definition : _ -> Just definition
          [] -> Nothing
  case classDefinition of
    Nothing -> failInfer (instanceDefSpan instance_) ("class not found: " <> instanceDefClass instance_)
    Just class_ -> do
      methods <- mapM (inferInstanceMethod class_ instance_) (instanceDefMethods instance_)
      pure (InferredInstance instance_ methods)

inferInstanceMethod :: ClassDef -> InstanceDef -> (ResolvedName, [RMatch]) -> Infer InferredMethod
inferInstanceMethod class_ instance_ (name, matches) = do
  let methodScheme =
        case lookup name (classDefMethods class_) of
          Just scheme_ -> scheme_
          Nothing -> error "internal inference error: missing class method scheme"
  (expectedType, expectedPredicates) <- instantiateClassMethod class_ instance_ methodScheme
  (typedMatches, inferredType, _) <-
    withGivens (instanceDefContext instance_ <> expectedPredicates) (inferMatchesFrom matches)
  unify (instanceDefSpan instance_) inferredType expectedType
  pure
    InferredMethod
      { inferredMethodName = name
      , inferredMethodScheme = methodScheme
      , inferredMethodMatches = typedMatches
      }

instantiateClassMethod :: ClassDef -> InstanceDef -> Scheme -> Infer (Type, [Pred])
instantiateClassMethod class_ instance_ scheme_@(Forall variables predicates body) = do
  let classVariable = classDefVar class_
      instanceHead = instanceDefHead instance_
      otherVariables = [variable | variable <- variables, variable /= classVariable]
  replacements <- mapM (freshType . tvKind) otherVariables
  let substitution = (classVariable, instanceHead) : zip otherVariables replacements
      expectedBody = applyType substitution body
      expectedPredicates =
        [ applyPred substitution predicate
        | predicate <- predicates
        , predClass predicate /= classDefName class_
        ]
  pure (expectedBody, expectedPredicates)

inferMatchesFrom :: [RMatch] -> Infer ([TEMatch], Type, [(TVar, Type)])
inferMatchesFrom matches = do
  results <- mapM inferMatch matches
  let types = [type_ | (_, type_) <- results]
  case types of
    [] -> failInfer NoSpan "method has no equations"
    first : rest -> mapM_ (unify NoSpan first) rest
  substitution <- currentSubstitution
  pure (map fst results, applyType substitution (head types), substitution)

-- REPL ----------------------------------------------------------------------

inferRepl :: CheckedModule -> RExpr -> Infer (Scheme, TExpr, [(EvidenceId, Pred)])
inferRepl _ expression = do
  mark <- length . isConstraints <$> getInferState
  (typed, type_) <- inferExpr expression
  substitution <- currentSubstitution
  constraints <- isConstraints <$> getInferState
  let newConstraints =
        [ applyPred substitution (constraintPredicate constraint)
        | constraint <- drop mark constraints
        ]
      finalType = applyType substitution type_
      quantified = nub (ftvType finalType)
      relevantPredicates =
        nub
          [ predicate
          | predicate <- newConstraints
          , let variables = ftvPred predicate
          , not (null variables)
          , all (`elem` quantified) variables
          ]
      concretePredicates =
        [ predicate
        | predicate <- newConstraints
        , null (ftvPred predicate)
        ]
  mapM_ (solvePredicate NoSpan) concretePredicates
  state <- getInferState
  pure (Forall quantified relevantPredicates finalType, typed, isEvidenceStore state)
