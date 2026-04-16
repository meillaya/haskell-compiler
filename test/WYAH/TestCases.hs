module WYAH.TestCases
  ( CaseGroup(..)
  , Case(..)
  , groupedCases
  ) where

import Data.List (isInfixOf)
import WYAH.Infer (inferExpr)
import WYAH.Name (renameExpr)
import WYAH.Parser (parseExpr)
import WYAH.Repl (runSource)
import WYAH.Span (renderSpan)
import WYAH.Syntax (Expr(..), Literal(..), exprSpan)
import WYAH.Type (normalizeTypeVariables, prettyType)

data CaseGroup = CaseGroup
  { groupName :: String
  , cases :: [Case]
  }

data Case
  = AssertionCase String Bool


groupedCases :: [CaseGroup]
groupedCases =
  [ CaseGroup "Parsing"
      [ AssertionCase "parse literal" parseLiteralHasSpan
      , AssertionCase "parse lambda spans binder/body" parseLambdaHasSpan
      , AssertionCase "parse malformed token stream reports location" parseErrorHasLocation
      , AssertionCase "parse let expression" parseLetExpression
      , AssertionCase "parse if expression" parseIfExpression
      , AssertionCase "parse boolean literal" parseBoolLiteral
      ]
  , CaseGroup "Renaming"
      [ AssertionCase "rename unbound variable reports source location" renameErrorHasLocation
      ]
  , CaseGroup "Inference"
      [ AssertionCase "builtin succ is in scope" (inferSource "succ" == Right "Int -> Int")
      , AssertionCase "builtin add is in scope" (inferSource "add" == Right "Int -> Int -> Int")
      , AssertionCase "infer id type" (inferSource "\\x -> x" == Right "t0 -> t0")
      , AssertionCase "infer const type" (inferSource "\\x -> \\y -> x" == Right "t0 -> t1 -> t0")
      , AssertionCase "infer boolean literal type" (inferSource "true" == Right "Bool")
      , AssertionCase "infer if expression type" (inferSource "if true then 1 else 0" == Right "Int")
      , AssertionCase "if branch mismatch fails" (leftContains "cannot unify Int with Bool" (inferSource "if true then 1 else false"))
      , AssertionCase "non-boolean condition fails" (leftContains "cannot unify Int with Bool" (inferSource "if 1 then 2 else 3"))
      , AssertionCase "occurs check failure" (leftContains "occurs check failed" (inferSource "\\x -> x x"))
      , AssertionCase "let-polymorphism via reused identity" (inferSource "let id = \\x -> x in let keep = id 42 in id (\\y -> y)" == Right "t0 -> t0")
      ]
  , CaseGroup "Evaluation"
      [ AssertionCase "pipeline application" (runSource "(\\x -> x) 42" == Right "type: Int\nvalue: VInt 42")
      , AssertionCase "let evaluation" (runSource "let id = \\x -> x in id 42" == Right "type: Int\nvalue: VInt 42")
      , AssertionCase "if evaluation true branch" (runSource "if true then 1 else 0" == Right "type: Int\nvalue: VInt 1")
      , AssertionCase "if evaluation false branch" (runSource "if false then 1 else 0" == Right "type: Int\nvalue: VInt 0")
      , AssertionCase "succ evaluation" (runSource "succ 0" == Right "type: Int\nvalue: VInt 1")
      , AssertionCase "pred/succ evaluation" (runSource "pred (succ 0)" == Right "type: Int\nvalue: VInt 0")
      , AssertionCase "iszero evaluation" (runSource "iszero 0" == Right "type: Bool\nvalue: VBool True")
      , AssertionCase "add evaluation" (runSource "add 2 3" == Right "type: Int\nvalue: VInt 5")
      , AssertionCase "primitive can drive conditionals" (runSource "if iszero 0 then 1 else 2" == Right "type: Int\nvalue: VInt 1")
      , AssertionCase "ill-typed application rejected before eval" (leftContains "cannot unify" (runSource "42 0"))
      ]
  ]

parseLiteralHasSpan :: Bool
parseLiteralHasSpan =
  case parseExpr "<test>" "42" of
    Right expr -> renderSpan (exprSpan expr) == "1:1-1:3"
    Left _ -> False

parseLambdaHasSpan :: Bool
parseLambdaHasSpan =
  case parseExpr "<test>" "\\x -> x" of
    Right expr ->
      renderSpan (exprSpan expr) == "1:1-1:8"
        && case expr of
          Lam _ _ body -> renderSpan (exprSpan body) == "1:7-1:8"
          _ -> False
    Left _ -> False

parseErrorHasLocation :: Bool
parseErrorHasLocation =
  case parseExpr "sample.wyah" "(" of
    Left err -> "sample.wyah:1:1-1:2:" `isInfixOf` show err
    Right _ -> False

parseLetExpression :: Bool
parseLetExpression =
  case parseExpr "<test>" "let id = \\x -> x in id 42" of
    Right expr ->
      renderSpan (exprSpan expr) == "1:1-1:26"
        && case expr of
          Let _ _ bound body ->
            renderSpan (exprSpan bound) == "1:10-1:17"
              && renderSpan (exprSpan body) == "1:21-1:26"
          _ -> False
    Left _ -> False

parseIfExpression :: Bool
parseIfExpression =
  case parseExpr "<test>" "if true then 1 else 0" of
    Right expr ->
      renderSpan (exprSpan expr) == "1:1-1:22"
        && case expr of
          If _ cond yes no ->
            renderSpan (exprSpan cond) == "1:4-1:8"
              && renderSpan (exprSpan yes) == "1:14-1:15"
              && renderSpan (exprSpan no) == "1:21-1:22"
          _ -> False
    Left _ -> False

parseBoolLiteral :: Bool
parseBoolLiteral =
  case parseExpr "<test>" "true" of
    Right (Lit _ (LitBool True)) -> True
    _ -> False

renameErrorHasLocation :: Bool
renameErrorHasLocation =
  case parseExpr "<test>" "\\x -> y" of
    Left _ -> False
    Right expr ->
      case renameExpr expr of
        Left err -> "1:7-1:8" `isInfixOf` show err
        Right _ -> False

inferSource :: String -> Either String String
inferSource input = do
  parsed <- eitherShow (parseExpr "<test>" input)
  renamed <- eitherShow (renameExpr parsed)
  prettyType . normalizeTypeVariables <$> eitherShow (inferExpr renamed)

leftContains :: String -> Either String String -> Bool
leftContains needle result =
  case result of
    Left message -> needle `isInfixOf` message
    Right _ -> False

eitherShow :: Show e => Either e a -> Either String a
eitherShow = either (Left . show) Right
