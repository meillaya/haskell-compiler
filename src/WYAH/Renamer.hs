-- | Name resolution for ProtoHaskell.
--
-- The renamer assigns every binder a unique 'ResolvedName', groups multiple
-- equations for the same function, resolves constructors and class methods,
-- and desugars surface-only sugar (lists, tuples, ranges, sections, record
-- selectors).  Type variable names are intentionally left alone: the kind
-- checker owns that namespace.
module WYAH.Renamer
  ( RenameError(..)
  , renameModule
  ) where

import Data.List (nub, (\\))
import WYAH.Diagnostic
import WYAH.Prelude
import WYAH.Renamed
import WYAH.Span
import WYAH.Syntax

data RenameError = RenameError
  { renameErrorSource :: FilePath
  , renameErrorSpan :: Span
  , renameErrorMessage :: String
  }
  deriving (Eq)

instance Show RenameError where
  show err =
    renderDiagnostic
      Diagnostic
        { diagnosticPhase = phaseName RenamePhase
        , diagnosticSource = Just (renameErrorSource err)
        , diagnosticSpan = Just (renameErrorSpan err)
        , diagnosticMessage = renameErrorMessage err
        }

data RenameState = RenameState
  { rsSource :: FilePath
  , rsNextId :: Int
  , rsTopValues :: [(Name, ResolvedName)]
  , rsTopConstructors :: [(Name, ResolvedName)]
  , rsLocalValues :: [(Name, ResolvedName)]
  }

newtype RenameM a = RenameM {runRenameM :: RenameState -> Either RenameError (a, RenameState)}

instance Functor RenameM where
  fmap f (RenameM action) = RenameM $ \state ->
    case action state of
      Left err -> Left err
      Right (value, state') -> Right (f value, state')

instance Applicative RenameM where
  pure value = RenameM $ \state -> Right (value, state)
  RenameM pf <*> RenameM pa = RenameM $ \state ->
    case pf state of
      Left err -> Left err
      Right (f, state') ->
        case pa state' of
          Left err -> Left err
          Right (value, state'') -> Right (f value, state'')

instance Monad RenameM where
  RenameM action >>= f = RenameM $ \state ->
    case action state of
      Left err -> Left err
      Right (value, state') -> runRenameM (f value) state'

getState :: RenameM RenameState
getState = RenameM $ \state -> Right (state, state)

modifyState :: (RenameState -> RenameState) -> RenameM ()
modifyState f = RenameM $ \state -> Right ((), f state)

failRename :: Span -> String -> RenameM a
failRename span_ message = RenameM $ \state ->
  Left
    RenameError
      { renameErrorSource = rsSource state
      , renameErrorSpan = span_
      , renameErrorMessage = message
      }

freshName :: Name -> RenameM ResolvedName
freshName name = do
  state <- getState
  let uniqueId = rsNextId state
  modifyState $ \current -> current {rsNextId = uniqueId + 1}
  pure (ResolvedName name uniqueId)

withLocalValues :: [(Name, ResolvedName)] -> RenameM a -> RenameM a
withLocalValues values action = do
  state <- getState
  let old = rsLocalValues state
  modifyState $ \current -> current {rsLocalValues = values}
  result <- action
  modifyState $ \current -> current {rsLocalValues = old}
  pure result

lookupValue :: Name -> RenameM (Maybe ResolvedName)
lookupValue name = do
  state <- getState
  pure
    ( case lookup name (rsLocalValues state) of
        Just resolved -> Just resolved
        Nothing -> lookup name (rsTopValues state)
    )

lookupConstructor :: Name -> RenameM (Maybe ResolvedName)
lookupConstructor name = do
  state <- getState
  pure (lookup name (rsTopConstructors state))

requireValue :: Span -> Name -> RenameM ResolvedName
requireValue span_ name = do
  found <- lookupValue name
  case found of
    Just resolved -> pure resolved
    Nothing -> failRename span_ ("not in scope: " <> name)

requireConstructor :: Span -> Name -> RenameM ResolvedName
requireConstructor span_ name = do
  found <- lookupConstructor name
  case found of
    Just resolved -> pure resolved
    Nothing -> failRename span_ ("constructor not in scope: " <> name)

-- Entry point ---------------------------------------------------------------

renameModule :: FilePath -> Module -> Either RenameError RModule
renameModule file parsedModule = do
  let initial =
        RenameState
          { rsSource = file
          , rsNextId = 0
          , rsTopValues = preludeValues
          , rsTopConstructors = preludeConstructors
          , rsLocalValues = []
          }
  (result, _) <- runRenameM (prepareAndRename parsedModule) initial
  pure result

prepareAndRename :: Module -> RenameM RModule
prepareAndRename parsedModule = do
  top <- prepareTopLevel parsedModule
  modifyState $ \state ->
    state
      { rsTopValues = tlValueNames top <> preludeValues
      , rsTopConstructors = tlConstructorNames top <> preludeConstructors
      }
  declarations <- renameTopLevelDeclarations top
  selectors <- buildSelectors top
  pure
    RModule
      { rModuleSpan = moduleSpan parsedModule
      , rModuleName = moduleName parsedModule
      , rModuleDecls = declarations <> selectors
      }

-- Prepass -------------------------------------------------------------------

data TopLevel = TopLevel
  { tlDeclarations :: [Decl]
  , tlFunctionGroups :: [(Name, [Decl])]
  , tlSignatures :: [(Name, Type)]
  , tlDataDecls :: [(Span, DataDecl)]
  , tlTypeSynonyms :: [(Span, TypeSyn)]
  , tlClassDecls :: [(Span, ClassDecl)]
  , tlInstanceDecls :: [(Span, InstDecl)]
  , tlValueNames :: [(Name, ResolvedName)]
  , tlConstructorNames :: [(Name, ResolvedName)]
  , tlDataNames :: [(Name, ResolvedName)]
  , tlClassMethods :: [(Name, Name, ResolvedName)]
  , tlFields :: [FieldInfo]
  }

data FieldInfo = FieldInfo
  { fieldInfoName :: Name
  , fieldInfoSpan :: Span
  , fieldInfoConstructor :: ResolvedName
  , fieldInfoConstructorSpan :: Span
  , fieldInfoFields :: [Type]
  , fieldInfoIndex :: Int
  , fieldInfoDataName :: Name
  }

prepareTopLevel :: Module -> RenameM TopLevel
prepareTopLevel parsedModule = do
  let declarations = moduleDecls parsedModule
  groups <- pure (groupFunctions declarations)
  signatures <- pure (collectSignatures declarations)
  let dataDecls = [(declSpan decl, data_) | decl@(DData _ data_) <- declarations]
      typeSyns = [(declSpan decl, synonym) | decl@(DType _ synonym) <- declarations]
      classDecls = [(declSpan decl, class_) | decl@(DClass _ class_) <- declarations]
      instanceDecls = [(declSpan decl, instance_) | decl@(DInst _ instance_) <- declarations]
  validateSignatures groups signatures
  validateTypeNamespace dataDecls typeSyns classDecls
  validateConstructors dataDecls
  classMethods <- collectClassMethods classDecls
  let functionNames = map fst groups
      methodNames = map fst classMethods
      fieldTriples = collectFieldNames dataDecls
      allValueNames = nub (functionNames <> methodNames <> [name | (name, _, _) <- fieldTriples])
  validateValueNamespace functionNames methodNames fieldTriples
  valueNames <- allocateNames allValueNames
  constructorNames <- allocateNames (collectConstructorNames dataDecls)
  dataNames <- allocateNames (map (dataDeclName . snd) dataDecls)
  let classMethodNames =
        [ (method, className, requireAllocated valueNames method)
        | (method, className) <- classMethods
        ]
      fields = makeFieldInfos dataDecls constructorNames
  pure
    TopLevel
      { tlDeclarations = declarations
      , tlFunctionGroups = groups
      , tlSignatures = signatures
      , tlDataDecls = dataDecls
      , tlTypeSynonyms = typeSyns
      , tlClassDecls = classDecls
      , tlInstanceDecls = instanceDecls
      , tlValueNames = valueNames
      , tlConstructorNames = constructorNames
      , tlDataNames = dataNames
      , tlClassMethods = classMethodNames
      , tlFields = fields
      }

groupFunctions :: [Decl] -> [(Name, [Decl])]
groupFunctions = foldl add []
  where
    add groups declaration@(DFun _ name _ _ _) =
      case break ((== name) . fst) groups of
        (before, (_, equations) : after) ->
          before <> [(name, equations <> [declaration])] <> after
        _ -> groups <> [(name, [declaration])]
    add groups _ = groups

collectSignatures :: [Decl] -> [(Name, Type)]
collectSignatures declarations =
  [ (name, type_)
  | DSig _ name type_ <- declarations
  ]

validateSignatures :: [(Name, [Decl])] -> [(Name, Type)] -> RenameM ()
validateSignatures groups signatures = do
  let defined = map fst groups
      counts = countOccurrences (map fst signatures)
  case [ name | (name, count) <- counts, count > 1 ] of
    duplicate : _ -> failRename NoSpan ("duplicate type signature for " <> duplicate)
    [] ->
      case [ name | (name, _) <- signatures, name `notElem` defined ] of
        missing : _ -> failRename NoSpan ("type signature for undefined value " <> missing)
        [] -> pure ()

validateTypeNamespace :: [(Span, DataDecl)] -> [(Span, TypeSyn)] -> [(Span, ClassDecl)] -> RenameM ()
validateTypeNamespace dataDecls typeSyns classDecls = do
  let names =
        [ (dataDeclName data_, span_)
        | (span_, data_) <- dataDecls
        ]
          <> [ (typeSynName synonym, span_)
             | (span_, synonym) <- typeSyns
             ]
          <> [ (classDeclName class_, span_)
             | (span_, class_) <- classDecls
             ]
  case firstDuplicate names of
    Just (name, span_) -> failRename span_ ("duplicate type or class name: " <> name)
    Nothing -> pure ()

validateConstructors :: [(Span, DataDecl)] -> RenameM ()
validateConstructors dataDecls = do
  let constructors =
        [ (conDeclName constructor, conDeclSpan constructor)
        | (_, data_) <- dataDecls
        , constructor <- dataDeclCons data_
        ]
  case firstDuplicate constructors of
    Just (name, span_) -> failRename span_ ("duplicate constructor name: " <> name)
    Nothing -> pure ()

collectClassMethods :: [(Span, ClassDecl)] -> RenameM [(Name, Name)]
collectClassMethods classDecls = do
  let methods =
        [ (name, classDeclName class_, declSpan decl)
        | (_, class_) <- classDecls
        , decl <- classDeclDecls class_
        , Just name <- [declName decl]
        ]
  case firstDuplicate [(name, span_) | (name, _, span_) <- methods] of
    Just (name, span_) -> failRename span_ ("duplicate class method name: " <> name)
    Nothing -> pure [(name, className) | (name, className, _) <- methods]

validateValueNamespace :: [Name] -> [Name] -> [(Name, Name, Span)] -> RenameM ()
validateValueNamespace functionNames methodNames fields = do
  case firstDuplicate [(name, NoSpan) | name <- functionNames <> methodNames] of
    Just (name, _) -> failRename NoSpan ("duplicate top-level value name: " <> name)
    Nothing -> pure ()
  case [ fieldName | (fieldName, _, _) <- fields, fieldName `elem` (functionNames <> methodNames) ] of
    fieldName : _ -> failRename NoSpan ("record field clashes with an existing value: " <> fieldName)
    [] -> pure ()
  let grouped =
        [ (fieldName, nub [dataName | (candidate, dataName, _) <- fields, candidate == fieldName])
        | fieldName <- nub [name | (name, _, _) <- fields]
        ]
  case
      [ (fieldName, span_)
      | (fieldName, dataNames) <- grouped
      , length dataNames > 1
      , (_, _, span_) <- take 1 [entry | entry@(name, _, _) <- fields, name == fieldName]
      ] of
    (fieldName, span_) : _ -> failRename span_ ("record field is defined in more than one datatype: " <> fieldName)
    [] -> pure ()

allocateNames :: [Name] -> RenameM [(Name, ResolvedName)]
allocateNames names = mapM allocate names
  where
    allocate name = do
      resolved <- freshName name
      pure (name, resolved)

requireAllocated :: [(Name, ResolvedName)] -> Name -> ResolvedName
requireAllocated environment name =
  case lookup name environment of
    Just resolved -> resolved
    Nothing -> ResolvedName name (-777777)

collectFieldNames :: [(Span, DataDecl)] -> [(Name, Name, Span)]
collectFieldNames dataDecls =
  [ (fieldName, dataDeclName data_, conDeclSpan constructor)
  | (_, data_) <- dataDecls
  , constructor <- dataDeclCons data_
  , (fieldName, _) <- conDeclRecord constructor
  ]

collectConstructorNames :: [(Span, DataDecl)] -> [Name]
collectConstructorNames dataDecls =
  [ conDeclName constructor
  | (_, data_) <- dataDecls
  , constructor <- dataDeclCons data_
  ]

makeFieldInfos :: [(Span, DataDecl)] -> [(Name, ResolvedName)] -> [FieldInfo]
makeFieldInfos dataDecls constructorNames =
  [ FieldInfo
      { fieldInfoName = fieldName
      , fieldInfoSpan = conDeclSpan constructor
      , fieldInfoConstructor = resolvedConstructor
      , fieldInfoConstructorSpan = conDeclSpan constructor
      , fieldInfoFields = conDeclFields constructor
      , fieldInfoIndex = index
      , fieldInfoDataName = dataDeclName data_
      }
  | (_, data_) <- dataDecls
  , constructor <- dataDeclCons data_
  , let resolvedConstructor =
          case lookup (conDeclName constructor) constructorNames of
            Just resolved -> resolved
            Nothing -> ResolvedName (conDeclName constructor) (-888888)
  , (index, (fieldName, _)) <- zip [0 ..] (conDeclRecord constructor)
  ]

countOccurrences :: [Name] -> [(Name, Int)]
countOccurrences names =
  [ (name, length (filter (== name) names))
  | name <- nub names
  ]

firstDuplicate :: [(Name, Span)] -> Maybe (Name, Span)
firstDuplicate entries =
  case [ (name, span_) | (name, span_) <- entries, count name > 1 ] of
    first : _ -> Just first
    [] -> Nothing
  where
    count name = length (filter ((== name) . fst) entries)

-- Top-level declarations ----------------------------------------------------

renameTopLevelDeclarations :: TopLevel -> RenameM [RDecl]
renameTopLevelDeclarations top = go (tlDeclarations top) []
  where
    go [] acc = pure (reverse acc)
    go (decl : rest) acc = case decl of
      DFun _ name _ _ _ ->
        case lookup name (tlFunctionGroups top) of
          Just equations ->
            case lookup name (alreadyRenamed acc) of
              Just _ -> go rest acc
              Nothing -> do
                resolved <- requireValue (declSpan decl) name
                declaration <- renameFunctionGroup resolved (lookupSignature name) equations
                go rest (RFunDecl declaration : acc)
          Nothing -> go rest acc
      DSig _ _ _ -> go rest acc
      DData span_ data_ -> do
        info <- renameDataInfo top span_ data_
        go rest (RDataDecl info : acc)
      DType span_ synonym -> do
        info <- renameTypeSynonym top span_ synonym
        go rest (RTypeSynDecl info : acc)
      DClass span_ class_ -> do
        info <- renameClassInfo top span_ class_
        go rest (RClassDecl info : acc)
      DInst span_ instance_ -> do
        info <- renameInstanceInfo top span_ instance_
        go rest (RInstanceDecl info : acc)
      DFixity _ _ -> go rest acc

    alreadyRenamed declarations = [(name, ()) | RFunDecl function <- declarations, let name = resolvedOriginal (rFunName function)]

    lookupSignature name = lookup name (tlSignatures top)

renameFunctionGroup :: ResolvedName -> Maybe Type -> [Decl] -> RenameM RFun
renameFunctionGroup resolved signature equations = do
  matches <- mapM renameEquation equations
  pure
    RFun
      { rFunSpan = case equations of
          first : _ -> declSpan first
          [] -> NoSpan
      , rFunName = resolved
      , rFunMatches = matches
      , rFunSignature = signature
      }
  where
    renameEquation (DFun span_ _ patterns rhs whereDecls) = do
      (renamedPatterns, bindings) <- renamePatterns patterns
      checkDuplicateBinders patterns bindings
      scope <- rsLocalValues <$> getState
      let scopeWithParameters = bindings <> scope
      (scopeWithWhere, localDeclarations) <- renameLocalDeclarations scopeWithParameters whereDecls
      renamedRhs <- withLocalValues scopeWithWhere (renameRhs rhs)
      pure
        RMatch
          { rMatchSpan = span_
          , rMatchPatterns = renamedPatterns
          , rMatchRhs = renamedRhs
          , rMatchWhere = localDeclarations
          }
    renameEquation (DSig span_ _ _) = failRename span_ "a type signature cannot appear inside a function group"
    renameEquation declaration = failRename (declSpan declaration) "unsupported local declaration"

checkDuplicateBinders :: [Pattern] -> [(Name, ResolvedName)] -> RenameM ()
checkDuplicateBinders patterns bindings = do
  let names = map fst bindings
  case firstDuplicate [(name, patternSpanOf name) | name <- names] of
    Just (name, _) -> failRename (patternSpanOf name) ("variable bound more than once in a pattern: " <> name)
    Nothing -> pure ()
  where
    patternSpanOf name =
      case [ patternSpan pattern_ | pattern_ <- patterns, name `elem` patternNames pattern_ ] of
        span_ : _ -> span_
        [] -> NoSpan
    patternNames (PVar _ name) = [name]
    patternNames (PAs _ name inner) = name : patternNames inner
    patternNames (PCon _ _ arguments) = concatMap patternNames arguments
    patternNames (PTuple _ patterns) = concatMap patternNames patterns
    patternNames (PList _ patterns) = concatMap patternNames patterns
    patternNames (PAnn _ pattern_ _) = patternNames pattern_
    patternNames _ = []

renameDataInfo :: TopLevel -> Span -> DataDecl -> RenameM DataInfo
renameDataInfo top span_ data_ = do
  if null (dataDeclDeriving data_)
    then pure ()
    else failRename span_ "deriving clauses are not supported yet"
  resolvedDataName <-
    case lookup (dataDeclName data_) (tlDataNames top) of
      Just resolved -> pure resolved
      Nothing -> failRename span_ ("internal renamer error: missing data name " <> dataDeclName data_)
  constructors <- mapM renameConstructor (dataDeclCons data_)
  pure
    DataInfo
      { dataInfoSpan = span_
      , dataInfoName = resolvedDataName
      , dataInfoVars = dataDeclVars data_
      , dataInfoCons = constructors
      , dataInfoDeriving = dataDeclDeriving data_
      }
  where
    renameConstructor constructor = do
      resolvedConstructor <-
        case lookup (conDeclName constructor) (tlConstructorNames top) of
          Just resolved -> pure resolved
          Nothing -> failRename (conDeclSpan constructor) ("internal renamer error: missing constructor " <> conDeclName constructor)
      pure
        ConInfo
          { conInfoSpan = conDeclSpan constructor
          , conInfoName = resolvedConstructor
          , conInfoFields = conDeclFields constructor
          , conInfoResult = conDeclResult constructor
          , conInfoRecord = conDeclRecord constructor
          }

renameTypeSynonym :: TopLevel -> Span -> TypeSyn -> RenameM TypeSynInfo
renameTypeSynonym _ span_ synonym =
  pure
    TypeSynInfo
      { typeSynInfoSpan = span_
      , typeSynInfoName = typeSynName synonym
      , typeSynInfoVars = typeSynVars synonym
      , typeSynInfoRhs = typeSynRhs synonym
      }

renameClassInfo :: TopLevel -> Span -> ClassDecl -> RenameM ClassInfo
renameClassInfo top span_ class_ = do
  methods <- mapM renameMethod (classDeclDecls class_)
  pure
    ClassInfo
      { classInfoSpan = span_
      , classInfoName = classDeclName class_
      , classInfoVar = classDeclVar class_
      , classInfoSupers = classDeclContext class_
      , classInfoMethods = methods
      }
  where
    renameMethod (DSig methodSpan methodName type_) = do
      resolved <-
        case lookup methodName (tlValueNames top) of
          Just name -> pure name
          Nothing -> failRename methodSpan ("internal renamer error: missing class method " <> methodName)
      pure
        ClassMethodInfo
          { classMethodSpan = methodSpan
          , classMethodName = resolved
          , classMethodType = type_
          }
    renameMethod (DFun methodSpan _ _ _ _) =
      failRename methodSpan "default methods in class declarations are not supported yet"
    renameMethod declaration =
      failRename (declSpan declaration) "only method signatures may appear in a class body"

renameInstanceInfo :: TopLevel -> Span -> InstDecl -> RenameM InstanceInfo
renameInstanceInfo top span_ instance_ = do
  methods <- mapM renameMethod (groupMethods (instDeclDecls instance_))
  let required =
        [ method
        | (method, className, _) <- tlClassMethods top
        , className == instDeclClass instance_
        ]
      provided = map (resolvedOriginal . fst) methods
      missing = required \\ provided
  if null missing
    then pure ()
    else failRename span_ ("missing instance methods: " <> unwords missing)
  pure
    InstanceInfo
      { instanceInfoSpan = span_
      , instanceInfoClass = instDeclClass instance_
      , instanceInfoHead = instDeclHead instance_
      , instanceInfoContext = instDeclContext instance_
      , instanceInfoMethods = methods
      }
  where
    classMethods =
      [ method
      | (method, className, _) <- tlClassMethods top
      , className == instDeclClass instance_
      ]
    groupMethods declarations = groupFunctions declarations
    renameMethod (name, equations) = do
      methodResolved <-
        case [ resolved | (method, resolved) <- tlValueNames top, method == name ] of
          resolved : _ -> pure resolved
          [] -> failRename (declSpan (head equations)) ("instance method not in scope: " <> name)
      if name `notElem` classMethods
        then failRename (declSpan (head equations)) ("method " <> name <> " does not belong to class " <> instDeclClass instance_)
        else do
          matches <- mapM renameInstanceEquation equations
          pure (methodResolved, matches)
    renameInstanceEquation (DSig signatureSpan _ _) =
      failRename signatureSpan "type signatures in instance bodies are not supported yet"
    renameInstanceEquation (DFun matchSpan _ patterns rhs whereDecls) = do
      (renamedPatterns, bindings) <- renamePatterns patterns
      checkDuplicateBinders patterns bindings
      (scopeWithWhere, localDeclarations) <- renameLocalDeclarations bindings whereDecls
      renamedRhs <- withLocalValues scopeWithWhere (renameRhs rhs)
      pure
        RMatch
          { rMatchSpan = matchSpan
          , rMatchPatterns = renamedPatterns
          , rMatchRhs = renamedRhs
          , rMatchWhere = localDeclarations
          }
    renameInstanceEquation declaration = failRename (declSpan declaration) "only function equations may appear in an instance body"

-- Record selectors ----------------------------------------------------------

buildSelectors :: TopLevel -> RenameM [RDecl]
buildSelectors top = do
  let grouped = groupFields (tlFields top)
  mapM buildSelector grouped
  where
    groupFields fields =
      [ (fieldName, entries)
      | fieldName <- nub (map fieldInfoName fields)
      , let entries = [entry | entry <- fields, fieldInfoName entry == fieldName]
      ]
    buildSelector (fieldName, entries) = do
      resolvedField <-
        case lookup fieldName (tlValueNames top) of
          Just resolved -> pure resolved
          Nothing -> failRename (fieldInfoSpan (head entries)) ("internal renamer error: missing selector " <> fieldName)
      matches <- mapM (buildMatch fieldName) entries
      pure
        ( RFunDecl
            RFun
              { rFunSpan = fieldInfoSpan (head entries)
              , rFunName = resolvedField
              , rFunMatches = matches
              , rFunSignature = Nothing
              }
        )
    buildMatch fieldName entry = do
      fieldVariable <- freshName (fieldName <> "_field")
      let wildcards =
            [ if index == fieldInfoIndex entry
                then RPVar (fieldInfoSpan entry) fieldVariable
                else RPWild (fieldInfoSpan entry)
            | index <- [0 .. length (fieldInfoFields entry) - 1]
            ]
          pattern_ =
            RPCon
              (fieldInfoConstructorSpan entry)
              (fieldInfoConstructor entry)
              wildcards
      pure
        RMatch
          { rMatchSpan = fieldInfoSpan entry
          , rMatchPatterns = [pattern_]
          , rMatchRhs = RPlainRhs (fieldInfoSpan entry) (RVar (fieldInfoSpan entry) fieldVariable)
          , rMatchWhere = []
          }

-- Local declarations --------------------------------------------------------

renameLocalDeclarations :: [(Name, ResolvedName)] -> [Decl] -> RenameM ([(Name, ResolvedName)], [RDecl])
renameLocalDeclarations scope declarations = do
  let functions = groupFunctions declarations
      signatures = collectSignatures declarations
  mapM_ (validateLocalDeclaration) declarations
  validateSignatures functions signatures
  localNames <- mapM allocateLocal (map fst functions)
  let scopeWithLocals = localNames <> scope
  renamed <- withLocalValues scopeWithLocals (mapM (renameLocalGroup localNames signatures) functions)
  pure (scopeWithLocals, renamed)
  where
    allocateLocal name = do
      resolved <- freshName name
      pure (name, resolved)
    validateLocalDeclaration (DFun _ _ _ _ _) = pure ()
    validateLocalDeclaration (DSig _ _ _) = pure ()
    validateLocalDeclaration declaration =
      failRename (declSpan declaration) "only functions and signatures may appear in a let or where block"
    renameLocalGroup localNames signatures (name, equations) = do
      let resolved =
            case lookup name localNames of
              Just value -> value
              Nothing -> ResolvedName name (-555555)
          signature = lookup name signatures
      renameFunctionGroupLet resolved signature equations
    renameFunctionGroupLet resolved signature equations = do
      matches <- mapM renameEquation equations
      pure
        ( RFunDecl
            RFun
              { rFunSpan = case equations of
                  first : _ -> declSpan first
                  [] -> NoSpan
              , rFunName = resolved
              , rFunMatches = matches
              , rFunSignature = signature
              }
        )
    renameEquation (DFun span_ _ patterns rhs whereDecls) = do
      (renamedPatterns, bindings) <- renamePatterns patterns
      checkDuplicateBinders patterns bindings
      outerScope <- rsLocalValues <$> getState
      (scopeWithWhere, localDeclarations) <- renameLocalDeclarations (bindings <> outerScope) whereDecls
      renamedRhs <- withLocalValues scopeWithWhere (renameRhs rhs)
      pure
        RMatch
          { rMatchSpan = span_
          , rMatchPatterns = renamedPatterns
          , rMatchRhs = renamedRhs
          , rMatchWhere = localDeclarations
          }
    renameEquation declaration = failRename (declSpan declaration) "unsupported local declaration"

-- Expressions ---------------------------------------------------------------

renameExpr :: Expr -> RenameM RExpr
renameExpr expression = case expression of
  EVar span_ name -> do
    resolved <- requireValue span_ name
    pure (RVar span_ resolved)
  ECon span_ name -> do
    resolved <- requireConstructor span_ name
    pure (RCon span_ resolved)
  ELit span_ literal -> pure (RLit span_ literal)
  EApp span_ function argument -> do
    renamedFunction <- renameExpr function
    renamedArgument <- renameExpr argument
    pure (RApp span_ renamedFunction renamedArgument)
  ELam span_ patterns body -> do
    (renamedPatterns, bindings) <- renamePatterns patterns
    checkDuplicateBinders patterns bindings
    case [name | RPVar _ name <- renamedPatterns] of
      binders
        | length binders == length renamedPatterns -> do
            scope <- rsLocalValues <$> getState
            renamedBody <- withLocalValues (bindings <> scope) (renameExpr body)
            pure (RLam span_ binders renamedBody)
      _ -> failRename span_ "lambda binders must be variables"
  ELet span_ declarations body -> do
    scope <- rsLocalValues <$> getState
    (scopeWithLocals, renamedDeclarations) <- renameLocalDeclarations scope declarations
    renamedBody <- withLocalValues scopeWithLocals (renameExpr body)
    pure (RLet span_ [bind | RFunDecl function <- renamedDeclarations, Just bind <- [functionToBind function]] renamedBody)
  EIf span_ condition trueBranch falseBranch -> do
    renamedCondition <- renameExpr condition
    renamedTrue <- renameExpr trueBranch
    renamedFalse <- renameExpr falseBranch
    pure (RIf span_ renamedCondition renamedTrue renamedFalse)
  ECase span_ scrutinee alternatives -> do
    renamedScrutinee <- renameExpr scrutinee
    renamedAlternatives <- mapM renameAlternative alternatives
    pure (RCase span_ renamedScrutinee renamedAlternatives)
  EDo span_ statements -> do
    scope <- rsLocalValues <$> getState
    (_, renamedStatements) <- renameStatements scope statements
    pure (RDo span_ renamedStatements)
  EComp span_ result statements -> do
    scope <- rsLocalValues <$> getState
    (scopeWithStatements, renamedStatements) <- renameStatements scope statements
    renamedResult <- withLocalValues scopeWithStatements (renameExpr result)
    pure (RComp span_ renamedResult renamedStatements)
  ETuple span_ expressions -> do
    renamedExpressions <- mapM renameExpr expressions
    constructor <- requireConstructor span_ (tupleName (length expressions))
    pure (applyMany span_ (RCon span_ constructor) renamedExpressions)
  EList span_ expressions -> do
    renamedExpressions <- mapM renameExpr expressions
    cons <- requireConstructor span_ ":"
    nil <- requireConstructor span_ "[]"
    pure (foldr (\element rest -> RApp span_ (RApp span_ (RCon span_ cons) element) rest) (RCon span_ nil) renamedExpressions)
  ERange span_ fromExpression stepExpression toExpression -> do
    renamedFrom <- renameExpr fromExpression
    case (stepExpression, toExpression) of
      (Nothing, Just upper) -> do
        renamedUpper <- renameExpr upper
        function <- requireValue span_ "enumFromTo"
        pure (applyMany span_ (RVar span_ function) [renamedFrom, renamedUpper])
      (Nothing, Nothing) -> do
        function <- requireValue span_ "enumFrom"
        pure (applyMany span_ (RVar span_ function) [renamedFrom])
      (Just step, Just upper) -> do
        renamedStep <- renameExpr step
        renamedUpper <- renameExpr upper
        function <- requireValue span_ "enumFromThenTo"
        pure (applyMany span_ (RVar span_ function) [renamedFrom, renamedStep, renamedUpper])
      (Just step, Nothing) -> do
        renamedStep <- renameExpr step
        function <- requireValue span_ "enumFromThen"
        pure (applyMany span_ (RVar span_ function) [renamedFrom, renamedStep])
  ESection span_ section -> renameSection span_ section
  EAnn span_ inner type_ -> do
    renamedInner <- renameExpr inner
    pure (RAnn span_ renamedInner type_)

functionToBind :: RFun -> Maybe RBind
functionToBind function =
  Just
    RBind
      { rBindSpan = rFunSpan function
      , rBindName = rFunName function
      , rBindMatches = rFunMatches function
      , rBindSignature = rFunSignature function
      }

renameSection :: Span -> Section -> RenameM RExpr
renameSection span_ section = case section of
  SectionName operator -> do
    function <- lookupValue operator
    case function of
      Just resolved -> pure (RVar span_ resolved)
      Nothing -> do
        constructor <- requireConstructor span_ operator
        pure (RCon span_ constructor)
  SectionLeft left operator -> do
    renamedLeft <- renameExpr left
    function <- operatorExpression span_ operator
    variable <- freshName "section"
    pure
      ( RLam
          span_
          [variable]
          ( applyMany
              span_
              function
              [renamedLeft, RVar span_ variable]
          )
      )
  SectionRight operator right -> do
    renamedRight <- renameExpr right
    function <- operatorExpression span_ operator
    variable <- freshName "section"
    pure
      ( RLam
          span_
          [variable]
          ( applyMany
              span_
              function
              [RVar span_ variable, renamedRight]
          )
      )

operatorExpression :: Span -> Name -> RenameM RExpr
operatorExpression span_ name = do
  value <- lookupValue name
  case value of
    Just resolved -> pure (RVar span_ resolved)
    Nothing -> do
      constructor <- requireConstructor span_ name
      pure (RCon span_ constructor)

applyMany :: Span -> RExpr -> [RExpr] -> RExpr
applyMany span_ function arguments = foldl step function arguments
  where
    step accumulated argument = RApp span_ accumulated argument

tupleName :: Int -> Name
tupleName size = "(" <> replicate (size - 1) ',' <> ")"

renameAlternative :: Alt -> RenameM RAlt
renameAlternative alternative = do
  (renamedPattern, bindings) <- renamePattern (altPattern alternative)
  checkDuplicateBinders [altPattern alternative] bindings
  scope <- rsLocalValues <$> getState
  (scopeWithWhere, localDeclarations) <- renameLocalDeclarations (bindings <> scope) (altWhere alternative)
  renamedRhs <- withLocalValues scopeWithWhere (renameRhs (altRhs alternative))
  pure
    RAlt
      { rAltSpan = altSpan alternative
      , rAltPattern = renamedPattern
      , rAltRhs = renamedRhs
      , rAltWhere = localDeclarations
      }

renameRhs :: Rhs -> RenameM RRhs
renameRhs rhs = case rhs of
  PlainRhs span_ expression -> RPlainRhs span_ <$> renameExpr expression
  GuardedRhs span_ guards -> do
    renamed <- mapM renameGuardClause guards
    pure (RGuardedRhs span_ renamed)
  where
    renameGuardClause (guard_, expression) = do
      (renamedGuard, bindings) <- renameGuard guard_
      scope <- rsLocalValues <$> getState
      renamedExpression <- withLocalValues (bindings <> scope) (renameExpr expression)
      pure (renamedGuard, renamedExpression)

renameGuard :: Guard -> RenameM (RGuard, [(Name, ResolvedName)])
renameGuard guard_ = case guard_ of
  GuardExpr span_ expression -> do
    renamed <- renameExpr expression
    pure (RGuardExpr span_ renamed, [])
  GuardPattern span_ pattern_ expression -> do
    renamedExpression <- renameExpr expression
    (renamedPattern, bindings) <- renamePattern pattern_
    checkDuplicateBinders [pattern_] bindings
    pure (RGuardPattern span_ renamedPattern renamedExpression, bindings)
  GuardList span_ guards -> do
    outerScope <- rsLocalValues <$> getState
    (renamed, bindings) <- go outerScope guards
    pure (RGuardList span_ renamed, bindings)
    where
      go _ [] = pure ([], [])
      go scope (guard_ : rest) = do
        (renamedGuard, bindings) <- withLocalValues scope (renameGuard guard_)
        (renamedRest, restBindings) <- go (bindings <> scope) rest
        pure (renamedGuard : renamedRest, bindings <> restBindings)

renameStatements :: [(Name, ResolvedName)] -> [Stmt] -> RenameM ([(Name, ResolvedName)], [RStmt])
renameStatements scope statements = go scope statements []
  where
    go current [] acc = pure (current, reverse acc)
    go current (statement : rest) acc = case statement of
      Generator span_ pattern_ expression -> do
        renamedExpression <- withLocalValues current (renameExpr expression)
        (renamedPattern, bindings) <- renamePattern pattern_
        checkDuplicateBinders [pattern_] bindings
        go (bindings <> current) rest (RGenerator span_ renamedPattern renamedExpression : acc)
      Qualifier span_ expression -> do
        renamedExpression <- withLocalValues current (renameExpr expression)
        go current rest (RQualifier span_ renamedExpression : acc)
      LetStmt span_ declarations -> do
        (scopeWithLocals, renamedDeclarations) <- renameLocalDeclarations current declarations
        let binds = [bind | RFunDecl function <- renamedDeclarations, Just bind <- [functionToBind function]]
        go scopeWithLocals rest (RLetStmt span_ binds : acc)

-- Patterns ------------------------------------------------------------------

renamePatterns :: [Pattern] -> RenameM ([RPat], [(Name, ResolvedName)])
renamePatterns patterns = do
  results <- mapM renamePattern patterns
  let renamed = map fst results
      bindings = concatMap snd results
  pure (renamed, bindings)

renamePattern :: Pattern -> RenameM (RPat, [(Name, ResolvedName)])
renamePattern pattern_ = case pattern_ of
  PVar span_ name -> do
    resolved <- freshName name
    pure (RPVar span_ resolved, [(name, resolved)])
  PWild span_ -> pure (RPWild span_, [])
  PLit span_ literal -> pure (RPLit span_ literal, [])
  PCon span_ name arguments -> do
    resolved <- requireConstructor span_ name
    results <- mapM renamePattern arguments
    pure (RPCon span_ resolved (map fst results), concatMap snd results)
  PTuple span_ patterns -> do
    resolved <- requireConstructor span_ (tupleName (length patterns))
    results <- mapM renamePattern patterns
    pure (RPCon span_ resolved (map fst results), concatMap snd results)
  PList span_ patterns -> do
    cons <- requireConstructor span_ ":"
    nil <- requireConstructor span_ "[]"
    results <- mapM renamePattern patterns
    let renamed = map fst results
        bindings = concatMap snd results
        listPattern = foldr (\element rest -> RPCon span_ cons [element, rest]) (RPCon span_ nil []) renamed
    pure (listPattern, bindings)
  PAs span_ name inner -> do
    resolved <- freshName name
    (renamedInner, bindings) <- renamePattern inner
    pure (RPAs span_ resolved renamedInner, (name, resolved) : bindings)
  PAnn span_ inner type_ -> do
    (renamedInner, bindings) <- renamePattern inner
    pure (RPAnn span_ renamedInner type_, bindings)
