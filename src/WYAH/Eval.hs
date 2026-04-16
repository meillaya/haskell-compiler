module WYAH.Eval
  ( Value(..)
  , EvalError(..)
  , evalExpr
  ) where

import WYAH.Core
import WYAH.Name
import WYAH.Syntax (Literal(..))

data Value
  = VInt Integer
  | VBool Bool
  | VClosure [(ResolvedName, Value)] ResolvedName CoreExpr
  deriving (Eq, Show)

data EvalError = EvalError String
  deriving (Eq, Show)

evalExpr :: CoreExpr -> Either EvalError Value
evalExpr = go []

go :: [(ResolvedName, Value)] -> CoreExpr -> Either EvalError Value
go env expr = case expr of
  CVar name ->
    case lookup name env of
      Just value -> Right value
      Nothing -> Left (EvalError ("internal evaluation error: unbound core variable " <> show name))
  CLit literal -> case literal of
    LitInt n -> Right (VInt n)
    LitBool b -> Right (VBool b)
  CLam name body -> Right (VClosure env name body)
  CApp fun arg -> do
    funValue <- go env fun
    argValue <- go env arg
    case funValue of
      VClosure closureEnv name body -> go ((name, argValue) : closureEnv) body
      VInt _ -> Left (EvalError "attempted to apply a non-function value")
      VBool _ -> Left (EvalError "attempted to apply a non-function value")
  CLet name bound body -> do
    boundValue <- go env bound
    go ((name, boundValue) : env) body
  CIf cond yes no -> do
    condValue <- go env cond
    case condValue of
      VBool True -> go env yes
      VBool False -> go env no
      _ -> Left (EvalError "if condition did not evaluate to a boolean")
