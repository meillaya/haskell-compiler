module WYAH.Core
  ( CoreExpr(..)
  , toCore
  ) where

import WYAH.Name
import WYAH.Syntax (Literal)

data CoreExpr
  = CVar ResolvedName
  | CLit Literal
  | CLam ResolvedName CoreExpr
  | CApp CoreExpr CoreExpr
  | CLet ResolvedName CoreExpr CoreExpr
  | CIf CoreExpr CoreExpr CoreExpr
  deriving (Eq, Show)

toCore :: RenamedExpr -> CoreExpr
toCore expr = case expr of
  RVar _ name -> CVar name
  RLit _ literal -> CLit literal
  RLam _ name body -> CLam name (toCore body)
  RApp _ fun arg -> CApp (toCore fun) (toCore arg)
  RLet _ name bound body -> CLet name (toCore bound) (toCore body)
  RIf _ cond yes no -> CIf (toCore cond) (toCore yes) (toCore no)
