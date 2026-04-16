module WYAH.Name
  ( ResolvedName(..)
  , RenamedExpr(..)
  , RenameError(..)
  , renameExpr
  ) where

import WYAH.Span (Span, renderSpan)
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

data RenameError = RenameError
  { renameErrorSpan :: Span
  , renameErrorMessage :: String
  }
  deriving (Eq, Show)

renameExpr :: Expr -> Either RenameError RenamedExpr
renameExpr expr = fst <$> go [] 0 expr

go :: [(Name, ResolvedName)] -> Int -> Expr -> Either RenameError (RenamedExpr, Int)
go env nextId expression =
  case expression of
    Var sp name ->
      case lookup name env of
        Just resolved -> Right (RVar sp resolved, nextId)
        Nothing -> Left (RenameError sp ("not in scope: " <> name <> " at " <> renderSpan sp))
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
