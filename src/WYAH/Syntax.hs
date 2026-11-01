-- | Surface syntax for ProtoHaskell.
--
-- This is the parser's output and the renamer's input.  Nothing here is
-- resolved: names are plain strings, type variables are plain strings, and
-- every node that can produce a diagnostic carries a 'Span'.
module WYAH.Syntax
  ( Name
  , Module(..)
  , Decl(..)
  , DataDecl(..)
  , ConDecl(..)
  , TypeSyn(..)
  , ClassDecl(..)
  , InstDecl(..)
  , FixitySpec(..)
  , Assoc(..)
  , Fixity(..)
  , FixityTable
  , Expr(..)
  , Section(..)
  , Rhs(..)
  , Guard(..)
  , Alt(..)
  , Stmt(..)
  , Pattern(..)
  , Literal(..)
  , Type(..)
  , Pred(..)
  , declSpan
  , declName
  , exprSpan
  , rhsSpan
  , guardSpan
  , stmtSpan
  , patternSpan
  , typeSpan
  , isConstructorName
  , isOperatorName
  , builtinFixities
  , lookupFixity
  , mkFixity
  ) where

import Data.Char (isAlpha, isUpper)
import Data.Maybe (fromMaybe)
import WYAH.Span (Span)

type Name = String

data Module = Module
  { moduleSpan :: Span
  , moduleName :: Maybe Name
  , moduleDecls :: [Decl]
  }
  deriving (Eq, Show)

data Decl
  = DFun Span Name [Pattern] Rhs [Decl]
  | DSig Span Name Type
  | DData Span DataDecl
  | DType Span TypeSyn
  | DClass Span ClassDecl
  | DInst Span InstDecl
  | DFixity Span FixitySpec
  deriving (Eq, Show)

data DataDecl = DataDecl
  { dataDeclSpan :: Span
  , dataDeclName :: Name
  , dataDeclVars :: [Name]
  , dataDeclCons :: [ConDecl]
  , dataDeclDeriving :: [Name]
  }
  deriving (Eq, Show)

data ConDecl = ConDecl
  { conDeclSpan :: Span
  , conDeclName :: Name
  , conDeclFields :: [Type]
  , conDeclResult :: Maybe Type
  , conDeclRecord :: [(Name, Type)]
  }
  deriving (Eq, Show)

data TypeSyn = TypeSyn
  { typeSynSpan :: Span
  , typeSynName :: Name
  , typeSynVars :: [Name]
  , typeSynRhs :: Type
  }
  deriving (Eq, Show)

data ClassDecl = ClassDecl
  { classDeclSpan :: Span
  , classDeclContext :: [Pred]
  , classDeclName :: Name
  , classDeclVar :: Name
  , classDeclDecls :: [Decl]
  }
  deriving (Eq, Show)

data InstDecl = InstDecl
  { instDeclSpan :: Span
  , instDeclContext :: [Pred]
  , instDeclClass :: Name
  , instDeclHead :: Type
  , instDeclDecls :: [Decl]
  }
  deriving (Eq, Show)

data Assoc = AssocLeft | AssocRight | AssocNone
  deriving (Eq, Ord, Show)

data Fixity
  = Infix Assoc Int
  | Prefix Int
  | Postfix Int
  deriving (Eq, Ord, Show)

data FixitySpec = FixitySpec
  { fixitySpecSpan :: Span
  , fixityAssoc :: Assoc
  , fixityPrecedence :: Int
  , fixityOps :: [Name]
  }
  deriving (Eq, Show)

-- | Operator name -> fixity.
type FixityTable = [(Name, Fixity)]

data Expr
  = EVar Span Name
  | ECon Span Name
  | ELit Span Literal
  | EApp Span Expr Expr
  | ELam Span [Pattern] Expr
  | ELet Span [Decl] Expr
  | EIf Span Expr Expr Expr
  | ECase Span Expr [Alt]
  | EDo Span [Stmt]
  | ETuple Span [Expr]
  | EList Span [Expr]
  | ERange Span Expr (Maybe Expr) (Maybe Expr)
  | EComp Span Expr [Stmt]
  | ESection Span Section
  | EAnn Span Expr Type
  deriving (Eq, Show)

data Section
  = SectionLeft Expr Name
  | SectionRight Name Expr
  | SectionName Name
  deriving (Eq, Show)

data Rhs
  = PlainRhs Span Expr
  | GuardedRhs Span [(Guard, Expr)]
  deriving (Eq, Show)

data Guard
  = GuardExpr Span Expr
  | GuardPattern Span Pattern Expr
  | GuardList Span [Guard]
  deriving (Eq, Show)

data Alt = Alt
  { altSpan :: Span
  , altPattern :: Pattern
  , altRhs :: Rhs
  , altWhere :: [Decl]
  }
  deriving (Eq, Show)

data Stmt
  = Generator Span Pattern Expr
  | Qualifier Span Expr
  | LetStmt Span [Decl]
  deriving (Eq, Show)

data Pattern
  = PVar Span Name
  | PWild Span
  | PLit Span Literal
  | PCon Span Name [Pattern]
  | PTuple Span [Pattern]
  | PList Span [Pattern]
  | PAs Span Name Pattern
  | PAnn Span Pattern Type
  deriving (Eq, Show)

data Literal
  = LInt Integer
  | LBool Bool
  | LChar Char
  | LString String
  deriving (Eq, Ord, Show)

data Type
  = TyCon Span Name
  | TyVar Span Name
  | TyApp Span Type Type
  | TyFun Span Type Type
  | TyForall Span [Name] Type
  | TyContext Span [Pred] Type
  | TyTuple Span [Type]
  | TyList Span Type
  deriving (Eq, Show)

data Pred = Pred
  { predSpan :: Span
  , predClass :: Name
  , predType :: Type
  }
  deriving (Eq, Show)

-- Accessors -----------------------------------------------------------------

declSpan :: Decl -> Span
declSpan decl = case decl of
  DFun sp _ _ _ _ -> sp
  DSig sp _ _ -> sp
  DData sp _ -> sp
  DType sp _ -> sp
  DClass sp _ -> sp
  DInst sp _ -> sp
  DFixity sp _ -> sp

declName :: Decl -> Maybe Name
declName decl = case decl of
  DFun _ name _ _ _ -> Just name
  DSig _ name _ -> Just name
  DData _ data_ -> Just (dataDeclName data_)
  DType _ synonym -> Just (typeSynName synonym)
  DClass _ class_ -> Just (classDeclName class_)
  DInst _ _ -> Nothing
  DFixity _ _ -> Nothing

exprSpan :: Expr -> Span
exprSpan expr = case expr of
  EVar sp _ -> sp
  ECon sp _ -> sp
  ELit sp _ -> sp
  EApp sp _ _ -> sp
  ELam sp _ _ -> sp
  ELet sp _ _ -> sp
  EIf sp _ _ _ -> sp
  ECase sp _ _ -> sp
  EDo sp _ -> sp
  ETuple sp _ -> sp
  EList sp _ -> sp
  ERange sp _ _ _ -> sp
  EComp sp _ _ -> sp
  ESection sp _ -> sp
  EAnn sp _ _ -> sp

rhsSpan :: Rhs -> Span
rhsSpan rhs = case rhs of
  PlainRhs sp _ -> sp
  GuardedRhs sp _ -> sp

guardSpan :: Guard -> Span
guardSpan guard = case guard of
  GuardExpr sp _ -> sp
  GuardPattern sp _ _ -> sp
  GuardList sp _ -> sp

stmtSpan :: Stmt -> Span
stmtSpan stmt = case stmt of
  Generator sp _ _ -> sp
  Qualifier sp _ -> sp
  LetStmt sp _ -> sp

patternSpan :: Pattern -> Span
patternSpan pattern_ = case pattern_ of
  PVar sp _ -> sp
  PWild sp -> sp
  PLit sp _ -> sp
  PCon sp _ _ -> sp
  PTuple sp _ -> sp
  PList sp _ -> sp
  PAs sp _ _ -> sp
  PAnn sp _ _ -> sp

typeSpan :: Type -> Span
typeSpan type_ = case type_ of
  TyCon sp _ -> sp
  TyVar sp _ -> sp
  TyApp sp _ _ -> sp
  TyFun sp _ _ -> sp
  TyForall sp _ _ -> sp
  TyContext sp _ _ -> sp
  TyTuple sp _ -> sp
  TyList sp _ -> sp

-- Fixities ------------------------------------------------------------------

-- | The Haskell 2010 default fixities for the operators this compiler knows
-- about, plus the Prelude operators used by the embedded prelude.
builtinFixities :: FixityTable
builtinFixities =
  [ (".", Infix AssocRight 9)
  , ("!!", Infix AssocLeft 9)
  , ("^", Infix AssocRight 8)
  , ("^^", Infix AssocRight 8)
  , ("**", Infix AssocRight 8)
  , ("*", Infix AssocLeft 7)
  , ("/", Infix AssocLeft 7)
  , ("div", Infix AssocLeft 7)
  , ("mod", Infix AssocLeft 7)
  , ("quot", Infix AssocLeft 7)
  , ("rem", Infix AssocLeft 7)
  , ("+", Infix AssocLeft 6)
  , ("-", Infix AssocLeft 6)
  , (":", Infix AssocRight 5)
  , ("++", Infix AssocRight 5)
  , ("==", Infix AssocNone 4)
  , ("/=", Infix AssocNone 4)
  , ("<", Infix AssocNone 4)
  , ("<=", Infix AssocNone 4)
  , (">", Infix AssocNone 4)
  , (">=", Infix AssocNone 4)
  , ("elem", Infix AssocNone 4)
  , ("notElem", Infix AssocNone 4)
  , ("&&", Infix AssocRight 3)
  , ("||", Infix AssocRight 2)
  , (">>", Infix AssocLeft 1)
  , (">>=", Infix AssocLeft 1)
  , ("=<<", Infix AssocRight 1)
  , ("$", Infix AssocRight 0)
  , ("$!", Infix AssocRight 0)
  , ("seq", Infix AssocRight 0)
  ]

lookupFixity :: FixityTable -> Name -> Fixity
lookupFixity table name = fromMaybe (Infix AssocLeft 9) (lookup name table)

mkFixity :: Assoc -> Int -> Fixity
mkFixity = Infix

-- Name classification -------------------------------------------------------

-- | A constructor name starts with an uppercase letter or with a colon.
isConstructorName :: Name -> Bool
isConstructorName name = case name of
  [] -> False
  (c : _) -> isUpper c || c == ':'

isOperatorName :: Name -> Bool
isOperatorName [] = False
isOperatorName (c : _) = not (isAlpha c || c == '_')
