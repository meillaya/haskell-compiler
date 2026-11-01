-- | The renamed ProtoHaskell AST.
--
-- Every binder is a 'ResolvedName' with a module-unique integer.  Surface
-- sugar that can be removed without type information (lists, tuples,
-- operator sections, string literals) is desugared by the renamer; pattern
-- matching compilation happens later so that the type checker still sees
-- constructor patterns.
module WYAH.Renamed
  ( ResolvedName(..)
  , RModule(..)
  , RDecl(..)
  , RFun(..)
  , RMatch(..)
  , RBind(..)
  , RAlt(..)
  , RExpr(..)
  , RStmt(..)
  , RPat(..)
  , RRhs(..)
  , RGuard(..)
  , DataInfo(..)
  , ConInfo(..)
  , TypeSynInfo(..)
  , ClassInfo(..)
  , ClassMethodInfo(..)
  , InstanceInfo(..)
  , RType
  , rExprSpan
  , rPatSpan
  , rRhsSpan
  , rDeclSpan
  , rGuardSpan
  , resolvedText
  , resolvedShow
  ) where

import WYAH.Span (Span)
import WYAH.Syntax (Literal, Name, Pred, Type)

-- | A source name resolved to a unique binder.
data ResolvedName = ResolvedName
  { resolvedOriginal :: Name
  , resolvedUnique :: Int
  }
  deriving (Eq, Ord)

instance Show ResolvedName where
  show = resolvedShow

resolvedText :: ResolvedName -> Name
resolvedText = resolvedOriginal

-- | A debug rendering that keeps the unique id visible.  User-facing
-- diagnostics use 'resolvedText'.
resolvedShow :: ResolvedName -> String
resolvedShow (ResolvedName name uniqueId)
  | uniqueId < 0 = name
  | otherwise = name <> "#" <> show uniqueId

newtype RType = RType {unRType :: Type}
  deriving (Eq, Show)

data RModule = RModule
  { rModuleSpan :: Span
  , rModuleName :: Maybe Name
  , rModuleDecls :: [RDecl]
  }
  deriving (Eq, Show)

data RDecl
  = RFunDecl RFun
  | RDataDecl DataInfo
  | RTypeSynDecl TypeSynInfo
  | RClassDecl ClassInfo
  | RInstanceDecl InstanceInfo
  deriving (Eq, Show)

data RFun = RFun
  { rFunSpan :: Span
  , rFunName :: ResolvedName
  , rFunMatches :: [RMatch]
  , rFunSignature :: Maybe Type
  }
  deriving (Eq, Show)

data RMatch = RMatch
  { rMatchSpan :: Span
  , rMatchPatterns :: [RPat]
  , rMatchRhs :: RRhs
  , rMatchWhere :: [RDecl]
  }
  deriving (Eq, Show)

data RBind = RBind
  { rBindSpan :: Span
  , rBindName :: ResolvedName
  , rBindMatches :: [RMatch]
  , rBindSignature :: Maybe Type
  }
  deriving (Eq, Show)

data RAlt = RAlt
  { rAltSpan :: Span
  , rAltPattern :: RPat
  , rAltRhs :: RRhs
  , rAltWhere :: [RDecl]
  }
  deriving (Eq, Show)

data RExpr
  = RVar Span ResolvedName
  | RCon Span ResolvedName
  | RLit Span Literal
  | RApp Span RExpr RExpr
  | RLam Span [ResolvedName] RExpr
  | RLet Span [RBind] RExpr
  | RIf Span RExpr RExpr RExpr
  | RCase Span RExpr [RAlt]
  | RAnn Span RExpr Type
  | RDo Span [RStmt]
  | RComp Span RExpr [RStmt]
  deriving (Eq, Show)

data RStmt
  = RGenerator Span RPat RExpr
  | RQualifier Span RExpr
  | RLetStmt Span [RBind]
  deriving (Eq, Show)

data RPat
  = RPVar Span ResolvedName
  | RPWild Span
  | RPLit Span Literal
  | RPCon Span ResolvedName [RPat]
  | RPAs Span ResolvedName RPat
  | RPAnn Span RPat Type
  deriving (Eq, Show)

data RRhs
  = RPlainRhs Span RExpr
  | RGuardedRhs Span [(RGuard, RExpr)]
  deriving (Eq, Show)

data RGuard
  = RGuardExpr Span RExpr
  | RGuardPattern Span RPat RExpr
  | RGuardList Span [RGuard]
  deriving (Eq, Show)

data DataInfo = DataInfo
  { dataInfoSpan :: Span
  , dataInfoName :: ResolvedName
  , dataInfoVars :: [Name]
  , dataInfoCons :: [ConInfo]
  , dataInfoDeriving :: [Name]
  }
  deriving (Eq, Show)

data ConInfo = ConInfo
  { conInfoSpan :: Span
  , conInfoName :: ResolvedName
  , conInfoFields :: [Type]
  , conInfoResult :: Maybe Type
  , conInfoRecord :: [(Name, Type)]
  }
  deriving (Eq, Show)

data TypeSynInfo = TypeSynInfo
  { typeSynInfoSpan :: Span
  , typeSynInfoName :: Name
  , typeSynInfoVars :: [Name]
  , typeSynInfoRhs :: Type
  }
  deriving (Eq, Show)

data ClassInfo = ClassInfo
  { classInfoSpan :: Span
  , classInfoName :: Name
  , classInfoVar :: Name
  , classInfoSupers :: [Pred]
  , classInfoMethods :: [ClassMethodInfo]
  }
  deriving (Eq, Show)

data ClassMethodInfo = ClassMethodInfo
  { classMethodSpan :: Span
  , classMethodName :: ResolvedName
  , classMethodType :: Type
  }
  deriving (Eq, Show)

data InstanceInfo = InstanceInfo
  { instanceInfoSpan :: Span
  , instanceInfoClass :: Name
  , instanceInfoHead :: Type
  , instanceInfoContext :: [Pred]
  , instanceInfoMethods :: [(ResolvedName, [RMatch])]
  }
  deriving (Eq, Show)

-- Accessors -----------------------------------------------------------------

rExprSpan :: RExpr -> Span
rExprSpan expression = case expression of
  RVar sp _ -> sp
  RCon sp _ -> sp
  RLit sp _ -> sp
  RApp sp _ _ -> sp
  RLam sp _ _ -> sp
  RLet sp _ _ -> sp
  RIf sp _ _ _ -> sp
  RCase sp _ _ -> sp
  RAnn sp _ _ -> sp
  RDo sp _ -> sp
  RComp sp _ _ -> sp

rPatSpan :: RPat -> Span
rPatSpan pattern_ = case pattern_ of
  RPVar sp _ -> sp
  RPWild sp -> sp
  RPLit sp _ -> sp
  RPCon sp _ _ -> sp
  RPAs sp _ _ -> sp
  RPAnn sp _ _ -> sp

rRhsSpan :: RRhs -> Span
rRhsSpan rhs = case rhs of
  RPlainRhs sp _ -> sp
  RGuardedRhs sp _ -> sp

rGuardSpan :: RGuard -> Span
rGuardSpan guard_ = case guard_ of
  RGuardExpr sp _ -> sp
  RGuardPattern sp _ _ -> sp
  RGuardList sp _ -> sp

rDeclSpan :: RDecl -> Span
rDeclSpan decl = case decl of
  RFunDecl function -> rFunSpan function
  RDataDecl info -> dataInfoSpan info
  RTypeSynDecl info -> typeSynInfoSpan info
  RClassDecl info -> classInfoSpan info
  RInstanceDecl info -> instanceInfoSpan info

