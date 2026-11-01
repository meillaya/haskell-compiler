-- | The Core language consumed by the interpreter.
--
-- Core is deliberately small.  Type-class evidence has already been made
-- explicit by the elaborator: dictionaries are ordinary values, method
-- selection is @CSelect@, and superclass selection uses the same node with a
-- superclass selector.
module WYAH.Core
  ( CoreExpr(..)
  , CoreAlt(..)
  , CorePat(..)
  , Selector(..)
  , CoreModule(..)
  , constructorArity
  , prettyCore
  ) where

import WYAH.Renamed (ResolvedName, resolvedShow)
import WYAH.Syntax (Literal(..), Name)

data CoreExpr
  = CVar ResolvedName
  | CLit Literal
  | CLam [ResolvedName] CoreExpr
  | CApp CoreExpr CoreExpr
  | CLet [(ResolvedName, CoreExpr)] CoreExpr
  | CLetRec [(ResolvedName, CoreExpr)] CoreExpr
  | CCase CoreExpr [CoreAlt] CoreExpr
  | CCon ResolvedName
  | CDict Name [(Selector, CoreExpr)]
  | CSelect CoreExpr Selector
  | CError String
  deriving (Eq, Show)

data CoreAlt = CoreAlt CorePat CoreExpr
  deriving (Eq, Show)

data CorePat
  = CPVar ResolvedName
  | CPWild
  | CPLit Literal
  | CPCon ResolvedName [CorePat]
  | CPAs ResolvedName CorePat
  deriving (Eq, Show)

data Selector
  = SelectorMethod ResolvedName
  | SelectorSuper Name
  deriving (Eq, Ord, Show)

data CoreModule = CoreModule
  { coreBindings :: [(ResolvedName, CoreExpr)]
  , coreConstructors :: [(ResolvedName, Int)]
  }
  deriving (Eq, Show)

constructorArity :: CoreModule -> ResolvedName -> Maybe Int
constructorArity coreModule name = lookup name (coreConstructors coreModule)

-- | A small Core pretty-printer used by @:core@ and by debugging dumps.
prettyCore :: CoreExpr -> String
prettyCore expression = case expression of
  CVar name -> resolvedShow name
  CLit (LInt value) -> show value
  CLit (LBool value) -> if value then "True" else "False"
  CLit (LChar value) -> show value
  CLit (LString value) -> show value
  CLam names body -> "\\" <> unwords (map resolvedShow names) <> " -> " <> prettyCore body
  CApp function argument -> "(" <> prettyCore function <> " " <> prettyCore argument <> ")"
  CLet bindings body ->
    "let { "
      <> unwords [resolvedShow name <> " = " <> prettyCore value | (name, value) <- bindings]
      <> " } in "
      <> prettyCore body
  CLetRec bindings body ->
    "letrec { "
      <> unwords [resolvedShow name <> " = " <> prettyCore value | (name, value) <- bindings]
      <> " } in "
      <> prettyCore body
  CCase scrutinee alternatives fallback ->
    "case "
      <> prettyCore scrutinee
      <> " of { "
      <> unwords
        [ prettyPattern pattern_ <> " -> " <> prettyCore body
        | CoreAlt pattern_ body <- alternatives
        ]
      <> "; _ -> "
      <> prettyCore fallback
      <> " }"
  CCon name -> resolvedShow name
  CDict className fields -> "dict " <> className <> " { " <> show (length fields) <> " fields }"
  CSelect dictionary selector -> "select " <> show selector <> " (" <> prettyCore dictionary <> ")"
  CError message -> "error " <> show message

prettyPattern :: CorePat -> String
prettyPattern pattern_ = case pattern_ of
  CPVar name -> resolvedShow name
  CPWild -> "_"
  CPLit (LInt value) -> show value
  CPLit (LBool value) -> if value then "True" else "False"
  CPLit (LChar value) -> show value
  CPLit (LString value) -> show value
  CPCon name [] -> resolvedShow name
  CPCon name arguments -> "(" <> resolvedShow name <> " " <> unwords (map prettyPattern arguments) <> ")"
  CPAs name inner -> resolvedShow name <> "@" <> prettyPattern inner
