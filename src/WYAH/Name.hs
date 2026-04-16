module WYAH.Name
  ( ResolvedName(..)
  , RenamedExpr(..)
  , RenamedDecl(..)
  , RenamedProgram(..)
  , RenameError(..)
  , builtinBindings
  , lowerProgram
  , renameExpr
  , renameProgram
  ) where

import WYAH.Diagnostic
import WYAH.Span (Span)
import WYAH.Syntax

data ResolvedName = ResolvedName
  { resolvedOriginal :: Name
  , resolvedUnique :: Int
  }
  deriving (Eq, Ord)

instance Show ResolvedName where
  show (ResolvedName name uniqueId) = name <> "#" <> show uniqueId

data RenamedExpr
  = RVar Span ResolvedName
  | RLit Span Literal
  | RLam Span ResolvedName RenamedExpr
  | RApp Span RenamedExpr RenamedExpr
  | RLet Span ResolvedName RenamedExpr RenamedExpr
  | RIf Span RenamedExpr RenamedExpr RenamedExpr
  deriving (Eq, Show)

data RenamedDecl = RDecl Span ResolvedName RenamedExpr
  deriving (Eq, Show)

data RenamedProgram = RenamedProgram [RenamedDecl] RenamedExpr
  deriving (Eq, Show)

data RenameError = RenameError
  { renameErrorSpan :: Span
  , renameErrorMessage :: String
  }
  deriving (Eq)

instance Show RenameError where
  show err =
    renderDiagnostic
      Diagnostic
        { diagnosticPhase = "rename"
        , diagnosticSource = Nothing
        , diagnosticSpan = Just (renameErrorSpan err)
        , diagnosticMessage = renameErrorMessage err
        }

builtinBindings :: [(Name, ResolvedName)]
builtinBindings =
  [ ("succ", ResolvedName "succ" (-1))
  , ("pred", ResolvedName "pred" (-2))
  , ("iszero", ResolvedName "iszero" (-3))
  , ("add", ResolvedName "add" (-4))
  ]

renameExpr :: Expr -> Either RenameError RenamedExpr
renameExpr expr = fst <$> go builtinBindings 0 expr

renameProgram :: Program -> Either RenameError RenamedProgram
renameProgram (Program decls finalExpr) = do
  (renamedDecls, env, nextId) <- renameDecls builtinBindings [] 0 decls
  (renamedFinal, _) <- go env nextId finalExpr
  pure (RenamedProgram renamedDecls renamedFinal)

lowerProgram :: RenamedProgram -> RenamedExpr
lowerProgram (RenamedProgram decls finalExpr) = foldr wrap finalExpr decls
  where
    wrap (RDecl sp name bound) body = RLet sp name bound body

renameDecls :: [(Name, ResolvedName)] -> [(Name, Span)] -> Int -> [Decl] -> Either RenameError ([RenamedDecl], [(Name, ResolvedName)], Int)
renameDecls env _ nextId [] = Right ([], env, nextId)
renameDecls env seen nextId (Decl sp name bound : rest) =
  case lookup name seen of
    Just firstSpan ->
      Left
        ( RenameError
            sp
            ( "duplicate top-level declaration for "
                <> name
                <> "; first declared at "
                <> show firstSpan
            )
        )
    Nothing -> do
      (renamedBound, nextId') <- go env nextId bound
      let resolved = ResolvedName name nextId'
          env' = (name, resolved) : env
          seen' = (name, sp) : seen
      (renamedRest, env'', nextId'') <- renameDecls env' seen' (nextId' + 1) rest
      pure (RDecl sp resolved renamedBound : renamedRest, env'', nextId'')

go :: [(Name, ResolvedName)] -> Int -> Expr -> Either RenameError (RenamedExpr, Int)
go env nextId expression =
  case expression of
    Var sp name ->
      case lookup name env of
        Just resolved -> Right (RVar sp resolved, nextId)
        Nothing -> Left (RenameError sp ("not in scope: " <> name))
    Lit sp literal -> Right (RLit sp literal, nextId)
    Lam sp binder body -> do
      let resolved = ResolvedName binder nextId
      (renamedBody, nextId') <- go ((binder, resolved) : env) (nextId + 1) body
      Right (RLam sp resolved renamedBody, nextId')
    App sp fun arg -> do
      (renamedFun, nextId') <- go env nextId fun
      (renamedArg, nextId'') <- go env nextId' arg
      Right (RApp sp renamedFun renamedArg, nextId'')
    Let sp binder bound body -> do
      (renamedBound, nextId') <- go env nextId bound
      let resolved = ResolvedName binder nextId'
      (renamedBody, nextId'') <- go ((binder, resolved) : env) (nextId' + 1) body
      Right (RLet sp resolved renamedBound renamedBody, nextId'')
    If sp cond yes no -> do
      (renamedCond, nextId') <- go env nextId cond
      (renamedYes, nextId'') <- go env nextId' yes
      (renamedNo, nextId''') <- go env nextId'' no
      Right (RIf sp renamedCond renamedYes renamedNo, nextId''')
