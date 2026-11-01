-- | The typed intermediate representation produced by inference.
--
-- Every expression carries its inferred type.  Variable occurrences also
-- carry evidence ids: one id per predicate in the instantiated scheme.  The
-- elaborator later replaces evidence ids with dictionary expressions built
-- from the surrounding binding's dictionary parameters or from instance
-- declarations.
module WYAH.Typed
  ( EvidenceId(..)
  , TExpr(..)
  , TEBind(..)
  , TEMatch(..)
  , TEAlt(..)
  , TRhs(..)
  , TRGuard(..)
  , tExprSpan
  , tRhsSpan
  ) where

import WYAH.Renamed (RPat, ResolvedName)
import WYAH.Span (Span(..))
import WYAH.Syntax (Literal)
import WYAH.Type (Scheme, Type)

newtype EvidenceId = EvidenceId Int
  deriving (Eq, Ord, Show)

data TExpr
  = TEVar Span ResolvedName Type [EvidenceId]
  | TECon Span ResolvedName Type
  | TELit Span Literal Type
  | TEApp Span TExpr TExpr Type
  | TELam Span ResolvedName Type TExpr Type
  | TELet Span [TEBind] TExpr Type
  | TEIf Span TExpr TExpr TExpr Type
  | TECase Span TExpr [TEAlt] Type
  deriving (Eq, Show)

data TEBind = TEBind
  { tBindSpan :: Span
  , tBindName :: ResolvedName
  , tBindScheme :: Scheme
  , tBindMatches :: [TEMatch]
  }
  deriving (Eq, Show)

data TEMatch = TEMatch
  { tMatchSpan :: Span
  , tMatchPatterns :: [RPat]
  , tMatchRhs :: TRhs
  , tMatchWhere :: [TEBind]
  }
  deriving (Eq, Show)

data TEAlt = TEAlt
  { tAltSpan :: Span
  , tAltPattern :: RPat
  , tAltRhs :: TRhs
  , tAltWhere :: [TEBind]
  }
  deriving (Eq, Show)

data TRhs
  = TRPlain TExpr
  | TRGuarded [(TRGuard, TExpr)]
  deriving (Eq, Show)

data TRGuard
  = TRGuardExpr TExpr
  | TRGuardPattern RPat TExpr
  | TRGuardList [TRGuard]
  deriving (Eq, Show)

tExprSpan :: TExpr -> Span
tExprSpan expression = case expression of
  TEVar sp _ _ _ -> sp
  TECon sp _ _ -> sp
  TELit sp _ _ -> sp
  TEApp sp _ _ _ -> sp
  TELam sp _ _ _ _ -> sp
  TELet sp _ _ _ -> sp
  TEIf sp _ _ _ _ -> sp
  TECase sp _ _ _ -> sp

tRhsSpan :: TRhs -> Span
tRhsSpan rhs = case rhs of
  TRPlain expression -> tExprSpan expression
  TRGuarded clauses ->
    case clauses of
      (_, expression) : _ -> tExprSpan expression
      [] -> NoSpan
