-- | Elaboration from the typed AST to Core.
--
-- This pass does three jobs:
--
-- * compile function equations and case alternatives into Core case trees,
-- * replace evidence ids with dictionary expressions,
-- * generate dictionary values for instances and selector functions for
--   class methods.
module WYAH.Elaborate
  ( ElaborateError(..)
  , elaborateModule
  , elaborateExpression
  ) where

import WYAH.Core
import WYAH.Diagnostic
import WYAH.Infer
import WYAH.Kind
import WYAH.Prelude (builtinName)
import WYAH.Renamed
import WYAH.Span
import WYAH.Syntax (Literal(..), Name)
import WYAH.Type
import WYAH.Typed

data ElaborateError = ElaborateError
  { elaborateErrorSource :: FilePath
  , elaborateErrorSpan :: Span
  , elaborateErrorMessage :: String
  }
  deriving (Eq)

instance Show ElaborateError where
  show err =
    renderDiagnostic
      Diagnostic
        { diagnosticPhase = phaseName DesugarPhase
        , diagnosticSource = Just (elaborateErrorSource err)
        , diagnosticSpan = Just (elaborateErrorSpan err)
        , diagnosticMessage = elaborateErrorMessage err
        }

data ElabEnv = ElabEnv
  { elabSource :: FilePath
  , elabEvidence :: [(EvidenceId, Pred)]
  , elabGivens :: [(Pred, CoreExpr)]
  , elabClasses :: [ClassDef]
  , elabInstances :: [(InstanceDef, ResolvedName)]
  , elabConstructors :: [(ResolvedName, Int)]
  }

data ElabState = ElabState
  { esNext :: Int
  , esSource :: FilePath
  }

newtype Elab a = Elab {runElab :: ElabState -> Either ElaborateError (a, ElabState)}

instance Functor Elab where
  fmap f (Elab action) = Elab $ \state ->
    case action state of
      Left err -> Left err
      Right (value, state') -> Right (f value, state')

instance Applicative Elab where
  pure value = Elab $ \state -> Right (value, state)
  Elab pf <*> Elab pa = Elab $ \state ->
    case pf state of
      Left err -> Left err
      Right (f, state') ->
        case pa state' of
          Left err -> Left err
          Right (value, state'') -> Right (f value, state'')

instance Monad Elab where
  Elab action >>= f = Elab $ \state ->
    case action state of
      Left err -> Left err
      Right (value, state') -> runElab (f value) state'

freshCoreName :: String -> Elab ResolvedName
freshCoreName base = Elab $ \state ->
  let uniqueId = esNext state
      name = ResolvedName base uniqueId
   in Right (name, state {esNext = uniqueId + 1})

failElab :: Span -> String -> Elab a
failElab span_ message = Elab $ \state ->
  Left
    ElaborateError
      { elaborateErrorSource = esSource state
      , elaborateErrorSpan = span_
      , elaborateErrorMessage = message
      }

-- Public entry points -------------------------------------------------------

elaborateModule :: CheckedModule -> InferredModule -> Either ElaborateError CoreModule
elaborateModule _ inferred = do
  let instanceNames =
        [ (instance_, ResolvedName ("$d" <> instanceDefClass instance_) (-20000 - index))
        | (index, instance_) <- zip [0 ..] (inferredInstanceDefs inferred)
        ]
      baseEnv =
        ElabEnv
          { elabSource = "<module>"
          , elabEvidence = inferredEvidence inferred
          , elabGivens = []
          , elabClasses = inferredClassDefs inferred
          , elabInstances = instanceNames
          , elabConstructors = constructorTable inferred
          }
      initialState = ElabState {esNext = 1000000, esSource = "<module>"}
  (bindings, _) <- runElab (elaborateBindings inferred baseEnv) initialState
  pure
    CoreModule
      { coreBindings = bindings
      , coreConstructors = constructorTable inferred
      }

elaborateExpression :: CheckedModule -> InferredModule -> Scheme -> TExpr -> Either ElaborateError CoreExpr
elaborateExpression _ inferred scheme_ expression = do
  let instanceNames =
        [ (instance_, ResolvedName ("$d" <> instanceDefClass instance_) (-20000 - index))
        | (index, instance_) <- zip [0 ..] (inferredInstanceDefs inferred)
        ]
      baseEnv =
        ElabEnv
          { elabSource = "<repl>"
          , elabEvidence = inferredEvidence inferred
          , elabGivens =
              [ (predicate, CVar (ResolvedName ("$d" <> predClass predicate) (-40000 - index)))
              | (index, predicate) <- zip [0 ..] (schemePredicates scheme_)
              , null (ftvPred predicate)
              ]
          , elabClasses = inferredClassDefs inferred
          , elabInstances = instanceNames
          , elabConstructors = constructorTable inferred
          }
      initialState = ElabState {esNext = 2000000, esSource = "<repl>"}
  (core, _) <- runElab (resolveExpr baseEnv expression) initialState
  pure core

constructorTable :: InferredModule -> [(ResolvedName, Int)]
constructorTable inferred =
  [ (conDefName constructor, length (conDefFields constructor))
  | definition <- inferredDataDefs inferred
  , constructor <- dataDefCons definition
  ]

-- Top-level elaboration -----------------------------------------------------

elaborateBindings :: InferredModule -> ElabEnv -> Elab [(ResolvedName, CoreExpr)]
elaborateBindings inferred env = do
  methodSelectors <- mapM (elaborateMethodSelector env) (allClassMethods (inferredClassDefs inferred))
  dictionaries <- mapM (elaborateInstance env inferred) (inferredInstances inferred)
  functions <- mapM (elaborateTopBinding env) (inferredBindings inferred)
  pure (methodSelectors <> dictionaries <> functions)
  where
    allClassMethods classes =
      [ (methodName, scheme_)
      | definition <- classes
      , (methodName, scheme_) <- classDefMethods definition
      ]

elaborateTopBinding :: ElabEnv -> TEBind -> Elab (ResolvedName, CoreExpr)
elaborateTopBinding env bind = do
  (dictParams, givens) <- dictionaryParameters (tBindScheme bind)
  core <- elaborateMatches (env {elabGivens = givens <> elabGivens env}) (tBindMatches bind)
  pure (tBindName bind, wrapDictionaryLambdas dictParams core)

wrapDictionaryLambdas :: [(Pred, ResolvedName)] -> CoreExpr -> CoreExpr
wrapDictionaryLambdas [] body = body
wrapDictionaryLambdas parameters body = CLam (map snd parameters) body

dictionaryParameters :: Scheme -> Elab ([(Pred, ResolvedName)], [(Pred, CoreExpr)])
dictionaryParameters scheme_ = do
  parameters <- mapM makeParameter (schemePredicates scheme_)
  pure (parameters, [(predicate, CVar name) | (predicate, name) <- parameters])
  where
    makeParameter predicate = do
      name <- freshCoreName ("$d" <> predClass predicate)
      pure (predicate, name)

elaborateMethodSelector :: ElabEnv -> (ResolvedName, Scheme) -> Elab (ResolvedName, CoreExpr)
elaborateMethodSelector _ (name, _) = do
  dictionary <- freshCoreName ("$d" <> resolvedOriginal name)
  pure (name, CLam [dictionary] (CSelect (CVar dictionary) (SelectorMethod name)))

elaborateInstance :: ElabEnv -> InferredModule -> InferredInstance -> Elab (ResolvedName, CoreExpr)
elaborateInstance env _ inferredInstance = do
  let definition = inferredInstanceDef inferredInstance
      instanceName =
        case lookup definition (elabInstances env) of
          Just name -> name
          Nothing -> ResolvedName ("$d" <> instanceDefClass definition) (-20001)
  contextParameters <- mapM makeContextParameter (instanceDefContext definition)
  self <- freshCoreName "$self"
  let contextGivens = [(predicate, CVar name) | (predicate, name) <- contextParameters]
      selfPredicate = Pred (instanceDefSpan definition) (instanceDefClass definition) (instanceDefHead definition)
      localEnv = env {elabGivens = (selfPredicate, CVar self) : contextGivens}
      superEnv = env {elabGivens = contextGivens}
  methods <- mapM (elaborateInstanceMethod localEnv definition) (inferredInstanceMethods inferredInstance)
  supers <- mapM (elaborateSuperclass superEnv) (superclassClosure (elabClasses env) (instanceDefClass definition) (instanceDefHead definition))
  let fields = methods <> supers
      dictionaryBody = CDict (instanceDefClass definition) fields
      selfBinding = CLetRec [(self, dictionaryBody)] (CVar self)
      body = wrapDictionaryLambdas contextParameters selfBinding
  pure (instanceName, body)
  where
    makeContextParameter predicate = do
      name <- freshCoreName ("$d" <> predClass predicate)
      pure (predicate, name)

    elaborateInstanceMethod localEnv definition method = do
      let scheme_ = inferredMethodScheme method
          (Forall _ predicates _) = scheme_
          extraPredicates =
            [ predicate
            | predicate <- predicates
            , predClass predicate /= instanceDefClass definition
            ]
      extraParameters <- mapM (\predicate -> do name <- freshCoreName ("$d" <> predClass predicate); pure (predicate, name)) extraPredicates
      let localEnv' =
            localEnv {elabGivens = [(predicate, CVar name) | (predicate, name) <- extraParameters] <> elabGivens localEnv}
      core <- elaborateMatches localEnv' (inferredMethodMatches method)
      pure (SelectorMethod (inferredMethodName method), wrapDictionaryLambdas extraParameters core)

    elaborateSuperclass superEnv (className, predicate) = do
      expression <- solveEvidence superEnv predicate
      pure (SelectorSuper className, expression)

-- Superclass helpers --------------------------------------------------------

superclassClosure :: [ClassDef] -> Name -> Type -> [(Name, Pred)]
superclassClosure classes className headType = go [] [className]
  where
    go _ [] = []
    go visited (current : rest)
      | current `elem` visited = go visited rest
      | otherwise =
          case lookup current [(classDefName definition, definition) | definition <- classes] of
            Nothing -> go (current : visited) rest
            Just definition ->
              let predicates =
                    [ substituteClassVariable (classDefVar definition) headType predicate
                    | predicate <- classDefSupers definition
                    ]
                  entries = [(predClass predicate, predicate) | predicate <- predicates]
               in entries <> go (current : visited) (map predClass predicates <> rest)

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

superclassPredicates :: ElabEnv -> Pred -> [Pred]
superclassPredicates env predicate =
  [ superPredicate
  | (className, superPredicate) <- superclassClosure (elabClasses env) (predClass predicate) (predType predicate)
  , className == predClass superPredicate
  ]

-- Matches and expressions ---------------------------------------------------

elaborateMatches :: ElabEnv -> [TEMatch] -> Elab CoreExpr
elaborateMatches _ [] = pure (CError "pattern match failure")
elaborateMatches env matches@(first : _) = do
  let arity = length (tMatchPatterns first)
  arguments <- mapM (\index -> freshCoreName ("$arg" <> show index)) [0 .. arity - 1]
  body <- compileMatches env matches arguments (CError "pattern match failure")
  pure (if arity == 0 then body else CLam arguments body)

compileMatches :: ElabEnv -> [TEMatch] -> [ResolvedName] -> CoreExpr -> Elab CoreExpr
compileMatches _ [] _ failure = pure failure
compileMatches env (match : rest) arguments failure = do
  restExpression <- compileMatches env rest arguments failure
  compileMatch env match arguments restExpression

compileMatch :: ElabEnv -> TEMatch -> [ResolvedName] -> CoreExpr -> Elab CoreExpr
compileMatch env match arguments failure = do
  localBindings <- mapM (elaborateLocalBinding env) (tMatchWhere match)
  rhs <- compileRhs env (tMatchRhs match) failure
  let body = if null localBindings then rhs else CLetRec localBindings rhs
  compilePatterns env (tMatchPatterns match) arguments body failure

elaborateLocalBinding :: ElabEnv -> TEBind -> Elab (ResolvedName, CoreExpr)
elaborateLocalBinding env bind = do
  (dictParams, givens) <- dictionaryParameters (tBindScheme bind)
  core <- elaborateMatches (env {elabGivens = givens <> elabGivens env}) (tBindMatches bind)
  pure (tBindName bind, wrapDictionaryLambdas dictParams core)

compilePatterns :: ElabEnv -> [RPat] -> [ResolvedName] -> CoreExpr -> CoreExpr -> Elab CoreExpr
compilePatterns _ [] [] body _ = pure body
compilePatterns env (pattern_ : patterns) (argument : arguments) body failure = do
  inner <- compilePatterns env patterns arguments body failure
  compilePattern env pattern_ argument (pure inner) failure
compilePatterns _ _ _ _ _ = failElab NoSpan "internal pattern arity mismatch"

compilePattern :: ElabEnv -> RPat -> ResolvedName -> Elab CoreExpr -> CoreExpr -> Elab CoreExpr
compilePattern env pattern_ argument body failure = case pattern_ of
  RPVar _ name -> do
    body' <- body
    pure (CLet [(name, CVar argument)] body')
  RPWild _ -> body
  RPLit _ literal -> do
    body' <- body
    pure (CCase (CVar argument) [CoreAlt (CPLit literal) body'] failure)
  RPCon _ name patterns -> do
    fieldNames <- mapM (\index -> freshCoreName ("$field" <> show index)) [0 .. length patterns - 1]
    body' <- body
    inner <- compilePatterns env patterns fieldNames body' failure
    pure (CCase (CVar argument) [CoreAlt (CPCon name (map CPVar fieldNames)) inner] failure)
  RPAs _ name innerPattern -> do
    body' <- body
    inner <- compilePattern env innerPattern argument (pure body') failure
    pure (CLet [(name, CVar argument)] inner)
  RPAnn _ inner _ -> do
    body' <- body
    compilePattern env inner argument (pure body') failure

compileRhs :: ElabEnv -> TRhs -> CoreExpr -> Elab CoreExpr
compileRhs env rhs failure = case rhs of
  TRPlain expression -> resolveExpr env expression
  TRGuarded clauses -> compileGuards env clauses failure

compileGuards :: ElabEnv -> [(TRGuard, TExpr)] -> CoreExpr -> Elab CoreExpr
compileGuards _ [] failure = pure failure
compileGuards env ((guard_, expression) : rest) failure = do
  restExpression <- compileGuards env rest failure
  case guard_ of
    TRGuardExpr guardExpression -> do
      guardCore <- resolveExpr env guardExpression
      body <- resolveExpr env expression
      pure
        ( CCase
            guardCore
            [CoreAlt (CPLit (LBool True)) body]
            restExpression
        )
    TRGuardPattern pattern_ scrutinee -> do
      scrutineeCore <- resolveExpr env scrutinee
      body <- resolveExpr env expression
      pure
        ( CCase
            scrutineeCore
            [CoreAlt (corePattern pattern_) body]
            restExpression
        )
    TRGuardList guards -> do
      body <- resolveExpr env expression
      compileGuardSequence env guards body restExpression

compileGuardSequence :: ElabEnv -> [TRGuard] -> CoreExpr -> CoreExpr -> Elab CoreExpr
compileGuardSequence _ [] body _ = pure body
compileGuardSequence env (guard_ : rest) body failure = case guard_ of
  TRGuardExpr expression -> do
    expressionCore <- resolveExpr env expression
    inner <- compileGuardSequence env rest body failure
    pure (CCase expressionCore [CoreAlt (CPLit (LBool True)) inner] failure)
  TRGuardPattern pattern_ scrutinee -> do
    scrutineeCore <- resolveExpr env scrutinee
    inner <- compileGuardSequence env rest body failure
    pure (CCase scrutineeCore [CoreAlt (corePattern pattern_) inner] failure)
  TRGuardList guards -> compileGuardSequence env (guards <> rest) body failure

resolveExpr :: ElabEnv -> TExpr -> Elab CoreExpr
resolveExpr env expression = case expression of
  TEVar span_ name _ evidence ->
    foldEvidence span_ (CVar name) evidence
  TECon _ name _ -> pure (CCon name)
  TELit _ (LString value) _ -> pure (stringCore value)
  TELit _ literal _ -> pure (CLit literal)
  TEApp _ function argument _ -> CApp <$> resolveExpr env function <*> resolveExpr env argument
  TELam _ name _ body _ -> CLam [name] <$> resolveExpr env body
  TELet _ bindings body _ -> do
    coreBindings <- mapM (elaborateLocalBinding env) bindings
    coreBody <- resolveExpr env body
    pure (CLetRec coreBindings coreBody)
  TEIf _ condition trueBranch falseBranch _ -> do
    coreCondition <- resolveExpr env condition
    coreTrue <- resolveExpr env trueBranch
    coreFalse <- resolveExpr env falseBranch
    pure
      ( CCase
          coreCondition
          [ CoreAlt (CPLit (LBool True)) coreTrue
          , CoreAlt (CPLit (LBool False)) coreFalse
          ]
          (CError "if condition was not a boolean")
      )
  TECase _ scrutinee alternatives _ -> do
    coreScrutinee <- resolveExpr env scrutinee
    compileCaseAlternatives env coreScrutinee alternatives
  where
    foldEvidence span_ base [] = pure base
    foldEvidence span_ base (evidenceId : rest) = do
      predicate <-
        case lookup evidenceId (elabEvidence env) of
          Just predicate -> pure predicate
          Nothing -> failElab span_ ("internal elaboration error: missing evidence " <> show evidenceId)
      dictionary <- solveEvidence env predicate
      foldEvidence span_ (CApp base dictionary) rest

compileCaseAlternatives :: ElabEnv -> CoreExpr -> [TEAlt] -> Elab CoreExpr
compileCaseAlternatives _ _ [] = pure (CError "pattern match failure")
compileCaseAlternatives env scrutinee (alternative : rest) = do
  restExpression <- compileCaseAlternatives env scrutinee rest
  localBindings <- mapM (elaborateLocalBinding env) (tAltWhere alternative)
  rhs <- compileRhs env (tAltRhs alternative) restExpression
  let body = if null localBindings then rhs else CLetRec localBindings rhs
  pure
    ( CCase
        scrutinee
        [CoreAlt (corePattern (tAltPattern alternative)) body]
        restExpression
    )

corePattern :: RPat -> CorePat
corePattern pattern_ = case pattern_ of
  RPVar _ name -> CPVar name
  RPWild _ -> CPWild
  RPLit _ literal -> CPLit literal
  RPCon _ name arguments -> CPCon name (map corePattern arguments)
  RPAs _ name inner -> CPAs name (corePattern inner)
  RPAnn _ inner _ -> corePattern inner

stringCore :: String -> CoreExpr
stringCore value =
  foldr
    (\character rest -> CApp (CApp (CCon (builtinName ":")) (CLit (LChar character))) rest)
    (CCon (builtinName "[]"))
    value

-- Evidence solving ----------------------------------------------------------

solveEvidence :: ElabEnv -> Pred -> Elab CoreExpr
solveEvidence env predicate = do
  case lookup predicate (elabGivens env) of
    Just expression -> pure expression
    Nothing -> do
      fromSuperclass <- solveFromGivenSuperclass env predicate
      case fromSuperclass of
        Just expression -> pure expression
        Nothing -> solveFromInstance env 0 predicate

solveFromGivenSuperclass :: ElabEnv -> Pred -> Elab (Maybe CoreExpr)
solveFromGivenSuperclass env predicate = go (elabGivens env)
  where
    go [] = pure Nothing
    go ((givenPredicate, expression) : rest)
      | predicate `elem` superclassPredicates env givenPredicate =
          pure (Just (CSelect expression (SelectorSuper (predClass predicate))))
      | otherwise = go rest

solveFromInstance :: ElabEnv -> Int -> Pred -> Elab CoreExpr
solveFromInstance env depth predicate
  | depth > 20 = failElab NoSpan ("instance search exceeded the recursion limit for " <> prettyPred predicate)
  | otherwise = do
      direct <- tryDirectInstances env depth predicate
      case direct of
        Just expression -> pure expression
        Nothing -> do
          indirect <- trySuperclassInstances env depth predicate
          case indirect of
            Just expression -> pure expression
            Nothing -> failElab NoSpan ("no instance for " <> prettyPred predicate)

tryDirectInstances :: ElabEnv -> Int -> Pred -> Elab (Maybe CoreExpr)
tryDirectInstances env depth predicate = go (elabInstances env)
  where
    go [] = pure Nothing
    go ((instance_, dictionaryName) : rest)
      | instanceDefClass instance_ /= predClass predicate = go rest
      | isBareInstanceVariable (instanceDefHead instance_) = go rest
      | otherwise =
          case matchInstanceHead (instanceDefHead instance_) (predType predicate) [] of
            Nothing -> go rest
            Just substitution -> do
              context <-
                mapM
                  (solveFromInstance env (depth + 1))
                  (map (applyPred substitution) (instanceDefContext instance_))
              pure (Just (foldl CApp (CVar dictionaryName) context))

trySuperclassInstances :: ElabEnv -> Int -> Pred -> Elab (Maybe CoreExpr)
trySuperclassInstances env depth predicate = go (elabInstances env)
  where
    go [] = pure Nothing
    go ((instance_, dictionaryName) : rest) =
      let closure = superclassClosure (elabClasses env) (instanceDefClass instance_) (instanceDefHead instance_)
       in if any (\(className, superPredicate) -> className == predClass predicate && superPredicate == predicate) closure
            then
              case matchInstanceHead (instanceDefHead instance_) (predType predicate) [] of
                Nothing -> go rest
                Just substitution -> do
                  context <-
                    mapM
                      (solveFromInstance env (depth + 1))
                      (map (applyPred substitution) (instanceDefContext instance_))
                  pure (Just (CSelect (foldl CApp (CVar dictionaryName) context) (SelectorSuper (predClass predicate))))
            else go rest

isBareInstanceVariable :: Type -> Bool
isBareInstanceVariable (TVariable _) = True
isBareInstanceVariable _ = False

matchInstanceHead :: Type -> Type -> [(TVar, Type)] -> Maybe [(TVar, Type)]
matchInstanceHead (TVariable variable) type_ substitution =
  case lookup variable substitution of
    Nothing -> Just ((variable, type_) : substitution)
    Just existing -> if existing == type_ then Just substitution else Nothing
matchInstanceHead (TCon left) (TCon right) substitution
  | tyConName left == tyConName right = Just substitution
  | otherwise = Nothing
matchInstanceHead (TApp leftFunction leftArgument) (TApp rightFunction rightArgument) substitution = do
  substitution' <- matchInstanceHead leftFunction rightFunction substitution
  matchInstanceHead leftArgument rightArgument substitution'
matchInstanceHead (TFun leftArgument leftResult) (TFun rightArgument rightResult) substitution = do
  substitution' <- matchInstanceHead leftArgument rightArgument substitution
  matchInstanceHead leftResult rightResult substitution'
matchInstanceHead _ _ _ = Nothing
