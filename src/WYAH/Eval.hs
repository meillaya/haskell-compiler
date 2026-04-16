module WYAH.Eval
  ( Value(..)
  , EvalError(..)
  , evalExpr
  ) where

import Data.Maybe (mapMaybe)
import WYAH.Core
import WYAH.Diagnostic
import WYAH.Name
import WYAH.Syntax (Literal(..))

data Primitive
  = PrimSucc
  | PrimPred
  | PrimIsZero
  | PrimAdd
  deriving (Eq, Show)

data Value
  = VInt Integer
  | VBool Bool
  | VClosure [(ResolvedName, Value)] ResolvedName CoreExpr
  | VPrimitive Primitive [Value]
  deriving (Eq, Show)

data EvalError = EvalError
  { evalErrorMessage :: String
  }
  deriving (Eq)

instance Show EvalError where
  show err =
    renderDiagnostic
      Diagnostic
        { diagnosticPhase = "eval"
        , diagnosticSource = Nothing
        , diagnosticSpan = Nothing
        , diagnosticMessage = evalErrorMessage err
        }

evalExpr :: CoreExpr -> Either EvalError Value
evalExpr = go preludeEnv

preludeEnv :: [(ResolvedName, Value)]
preludeEnv = mapMaybe builtinValue builtinBindings
  where
    builtinValue (_, resolved) =
      case resolvedOriginal resolved of
        "succ" -> Just (resolved, VPrimitive PrimSucc [])
        "pred" -> Just (resolved, VPrimitive PrimPred [])
        "iszero" -> Just (resolved, VPrimitive PrimIsZero [])
        "add" -> Just (resolved, VPrimitive PrimAdd [])
        _ -> Nothing

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
    applyValue funValue argValue
  CLet name bound body -> do
    boundValue <- go env bound
    go ((name, boundValue) : env) body
  CIf cond yes no -> do
    condValue <- go env cond
    case condValue of
      VBool True -> go env yes
      VBool False -> go env no
      _ -> Left (EvalError "if condition did not evaluate to a boolean")

applyValue :: Value -> Value -> Either EvalError Value
applyValue funValue argValue = case funValue of
  VClosure closureEnv name body -> go ((name, argValue) : closureEnv) body
  VPrimitive primitive args -> applyPrimitive primitive (args <> [argValue])
  VInt _ -> Left (EvalError "attempted to apply a non-function value")
  VBool _ -> Left (EvalError "attempted to apply a non-function value")

applyPrimitive :: Primitive -> [Value] -> Either EvalError Value
applyPrimitive primitive args = case (primitive, args) of
  (PrimSucc, [VInt n]) -> Right (VInt (n + 1))
  (PrimSucc, [_]) -> Left (EvalError "succ expects an Int")
  (PrimSucc, partial) -> Right (VPrimitive PrimSucc partial)
  (PrimPred, [VInt n]) -> Right (VInt (n - 1))
  (PrimPred, [_]) -> Left (EvalError "pred expects an Int")
  (PrimPred, partial) -> Right (VPrimitive PrimPred partial)
  (PrimIsZero, [VInt n]) -> Right (VBool (n == 0))
  (PrimIsZero, [_]) -> Left (EvalError "iszero expects an Int")
  (PrimIsZero, partial) -> Right (VPrimitive PrimIsZero partial)
  (PrimAdd, [VInt a, VInt b]) -> Right (VInt (a + b))
  (PrimAdd, [VInt _]) -> Right (VPrimitive PrimAdd args)
  (PrimAdd, [_]) -> Left (EvalError "add expects Int arguments")
  (PrimAdd, [_, _]) -> Left (EvalError "add expects Int arguments")
  (PrimAdd, partial) -> Right (VPrimitive PrimAdd partial)
