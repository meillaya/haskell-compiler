module WYAH.Syntax
  ( Name
  , Literal(..)
  , Expr(..)
  , Decl(..)
  , Program(..)
  , declAnnotation
  , declName
  , declSpan
  , declValue
  , exprSpan
  , programToExpr
  ) where

import WYAH.Span (Span)
import WYAH.Type (Type)

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

data Decl = Decl Span Name (Maybe Type) Expr
  deriving (Eq, Show)

data Program = Program [Decl] Expr
  deriving (Eq, Show)

declAnnotation :: Decl -> Maybe Type
declAnnotation (Decl _ _ ann _) = ann

declName :: Decl -> Name
declName (Decl _ name _ _) = name

declSpan :: Decl -> Span
declSpan (Decl sp _ _ _) = sp

declValue :: Decl -> Expr
declValue (Decl _ _ _ expr) = expr

exprSpan :: Expr -> Span
exprSpan expr = case expr of
  Var sp _ -> sp
  Lit sp _ -> sp
  Lam sp _ _ -> sp
  App sp _ _ -> sp
  Let sp _ _ _ -> sp
  If sp _ _ _ -> sp

programToExpr :: Program -> Expr
programToExpr (Program decls finalExpr) = foldr wrap finalExpr decls
  where
    wrap (Decl sp name _ bound) body = Let sp name bound body
