module WYAH.Syntax
  ( Name
  , Literal(..)
  , Expr(..)
  , exprSpan
  ) where

import WYAH.Span (Span)

type Name = String

data Literal
  = LitInt Integer
  | LitBool Bool
  deriving (Eq, Show)

data Expr
  = Var Span Name
  | Lit Span Literal
  | Lam Span Name Expr
  | App Span Expr Expr
  | Let Span Name Expr Expr
  | If Span Expr Expr Expr
  deriving (Eq, Show)

exprSpan :: Expr -> Span
exprSpan expr = case expr of
  Var sp _ -> sp
  Lit sp _ -> sp
  Lam sp _ _ -> sp
  App sp _ _ -> sp
  Let sp _ _ _ -> sp
  If sp _ _ _ -> sp
