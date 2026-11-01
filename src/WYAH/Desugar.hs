-- | Surface desugaring that must happen before inference.
--
-- The renamer resolves names; this pass removes the remaining expression
-- sugar: @do@ notation and list comprehensions.  Guards and pattern matching
-- are retained so that the type checker can still see constructors.
module WYAH.Desugar
  ( desugarModule
  ) where

import WYAH.Prelude (builtinName)
import WYAH.Renamed
import WYAH.Span
import WYAH.Syntax (Literal(..), Name)

newtype DS a = DS {runDS :: Int -> (a, Int)}

instance Functor DS where
  fmap f (DS action) = DS $ \next -> let (value, next') = action next in (f value, next')

instance Applicative DS where
  pure value = DS $ \next -> (value, next)
  DS pf <*> DS pa = DS $ \next ->
    let (f, next') = pf next
        (value, next'') = pa next'
     in (f value, next'')

instance Monad DS where
  DS action >>= f = DS $ \next ->
    let (value, next') = action next
     in runDS (f value) next'

freshName :: Name -> DS ResolvedName
freshName base = DS $ \next -> (ResolvedName base next, next + 1)

desugarModule :: RModule -> RModule
desugarModule rModule =
  let (declarations, _) = runDS (mapM desugarDecl (rModuleDecls rModule)) (-500000)
   in rModule {rModuleDecls = declarations}

desugarDecl :: RDecl -> DS RDecl
desugarDecl declaration = case declaration of
  RFunDecl function -> RFunDecl <$> desugarFunction function
  RInstanceDecl info -> do
    methods <- mapM (\(name, matches) -> (,) name <$> mapM desugarMatch matches) (instanceInfoMethods info)
    pure (RInstanceDecl info {instanceInfoMethods = methods})
  other -> pure other

desugarFunction :: RFun -> DS RFun
desugarFunction function = do
  matches <- mapM desugarMatch (rFunMatches function)
  pure function {rFunMatches = matches}

desugarMatch :: RMatch -> DS RMatch
desugarMatch match = do
  rhs <- desugarRhs (rMatchRhs match)
  whereDeclarations <- mapM desugarDecl (rMatchWhere match)
  pure match {rMatchRhs = rhs, rMatchWhere = whereDeclarations}

desugarAlt :: RAlt -> DS RAlt
desugarAlt alternative = do
  rhs <- desugarRhs (rAltRhs alternative)
  whereDeclarations <- mapM desugarDecl (rAltWhere alternative)
  pure alternative {rAltRhs = rhs, rAltWhere = whereDeclarations}

desugarRhs :: RRhs -> DS RRhs
desugarRhs rhs = case rhs of
  RPlainRhs span_ expression -> RPlainRhs span_ <$> desugarExpr expression
  RGuardedRhs span_ clauses -> do
    clauses' <- mapM (\(guard_, expression) -> (,) <$> desugarGuard guard_ <*> desugarExpr expression) clauses
    pure (RGuardedRhs span_ clauses')

desugarGuard :: RGuard -> DS RGuard
desugarGuard guard_ = case guard_ of
  RGuardExpr span_ expression -> RGuardExpr span_ <$> desugarExpr expression
  RGuardPattern span_ pattern_ expression -> RGuardPattern span_ pattern_ <$> desugarExpr expression
  RGuardList span_ guards -> RGuardList span_ <$> mapM desugarGuard guards

desugarExpr :: RExpr -> DS RExpr
desugarExpr expression = case expression of
  RVar {} -> pure expression
  RCon {} -> pure expression
  RLit {} -> pure expression
  RApp span_ function argument -> RApp span_ <$> desugarExpr function <*> desugarExpr argument
  RLam span_ names body -> RLam span_ names <$> desugarExpr body
  RLet span_ bindings body -> do
    bindings' <- mapM desugarBind bindings
    RLet span_ bindings' <$> desugarExpr body
  RIf span_ condition trueBranch falseBranch ->
    RIf span_ <$> desugarExpr condition <*> desugarExpr trueBranch <*> desugarExpr falseBranch
  RCase span_ scrutinee alternatives -> do
    scrutinee' <- desugarExpr scrutinee
    alternatives' <- mapM desugarAlt alternatives
    pure (RCase span_ scrutinee' alternatives')
  RAnn span_ inner type_ -> RAnn span_ <$> desugarExpr inner <*> pure type_
  RDo span_ statements -> desugarDo span_ statements
  RComp span_ result statements -> desugarComprehension span_ result statements

desugarBind :: RBind -> DS RBind
desugarBind bind = do
  matches <- mapM desugarMatch (rBindMatches bind)
  pure bind {rBindMatches = matches}

desugarDo :: Span -> [RStmt] -> DS RExpr
desugarDo span_ statements = case statements of
  [] -> pure (failureExpression span_ "empty do block")
  [RQualifier _ expression] -> desugarExpr expression
  (RQualifier qualifierSpan expression : rest) -> do
    expression' <- desugarExpr expression
    rest' <- desugarDo span_ rest
    pure (application2 qualifierSpan (variable qualifierSpan ">>") expression' rest')
  (RGenerator generatorSpan pattern_ expression : rest) -> do
    expression' <- desugarExpr expression
    rest' <- desugarDo span_ rest
    lambda <- generatorLambda generatorSpan pattern_ rest'
    pure (application2 generatorSpan (variable generatorSpan ">>=") expression' lambda)
  (RLetStmt letSpan bindings : rest) -> do
    bindings' <- mapM desugarBind bindings
    rest' <- desugarDo span_ rest
    pure (RLet letSpan bindings' rest')

desugarComprehension :: Span -> RExpr -> [RStmt] -> DS RExpr
desugarComprehension span_ result statements = do
  result' <- desugarExpr result
  case statements of
    [] -> pure (consExpression span_ result' (nilExpression span_))
    (RGenerator generatorSpan pattern_ expression : rest) -> do
      expression' <- desugarExpr expression
      inner <- desugarComprehension span_ result rest
      variableName <- freshName "$comprehension"
      let lambda =
            RLam
              generatorSpan
              [variableName]
              ( RCase
                  generatorSpan
                  (RVar generatorSpan variableName)
                  [ RAlt generatorSpan pattern_ (RPlainRhs generatorSpan inner) []
                  , RAlt generatorSpan (RPWild generatorSpan) (RPlainRhs generatorSpan (nilExpression generatorSpan)) []
                  ]
              )
      pure (application2 generatorSpan (variable generatorSpan "concatMap") lambda expression')
    (RQualifier qualifierSpan condition : rest) -> do
      condition' <- desugarExpr condition
      inner <- desugarComprehension span_ result rest
      pure (RIf qualifierSpan condition' inner (nilExpression qualifierSpan))
    (RLetStmt letSpan bindings : rest) -> do
      bindings' <- mapM desugarBind bindings
      inner <- desugarComprehension span_ result rest
      pure (RLet letSpan bindings' inner)

generatorLambda :: Span -> RPat -> RExpr -> DS RExpr
generatorLambda span_ pattern_ body = case pattern_ of
  RPVar _ name -> pure (RLam span_ [name] body)
  _ -> do
    variableName <- freshName "$do"
    failure <- pure (failureExpression span_ "pattern match failure in do block")
    pure
      ( RLam
          span_
          [variableName]
          ( RCase
              span_
              (RVar span_ variableName)
              [ RAlt span_ pattern_ (RPlainRhs span_ body) []
              , RAlt span_ (RPWild span_) (RPlainRhs span_ failure) []
              ]
          )
      )

variable :: Span -> Name -> RExpr
variable span_ name = RVar span_ (builtinName name)

nilExpression :: Span -> RExpr
nilExpression span_ = RCon span_ (builtinName "[]")

consExpression :: Span -> RExpr -> RExpr -> RExpr
consExpression span_ headExpression tailExpression =
  application2 span_ (RCon span_ (builtinName ":")) headExpression tailExpression

failureExpression :: Span -> String -> RExpr
failureExpression span_ message =
  RApp
    span_
    (RVar span_ (builtinName "fail"))
    (RLit span_ (LString message))

application2 :: Span -> RExpr -> RExpr -> RExpr -> RExpr
application2 span_ function first second =
  RApp span_ (RApp span_ function first) second
