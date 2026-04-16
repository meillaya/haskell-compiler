module WYAH.TestCases
  ( CaseGroup(..)
  , Case(..)
  , groupedCases
  ) where

import Data.List (groupBy, isInfixOf, sortOn)
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

inlineGroups :: [CaseGroup]
inlineGroups =
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
  ]

groupedCases :: IO [CaseGroup]
groupedCases = do
  fixtureGroups <- loadFixtureGroups
  pure (inlineGroups <> fixtureGroups)

loadFixtureGroups :: IO [CaseGroup]
loadFixtureGroups = do
  rows <- concat <$> mapM loadFixtureFile fixtureFiles
  let sortedRows = sortOn (\(group, _, _) -> group) rows
  pure
    [ CaseGroup group [AssertionCase label result | (_, label, result) <- grouped]
    | grouped@((group, _, _) : _) <- groupBy sameGroup sortedRows
    ]
  where
    fixtureFiles =
      [ "test/fixtures/parser.tsv"
      , "test/fixtures/rename.tsv"
      , "test/fixtures/infer.tsv"
      , "test/fixtures/eval.tsv"
      ]
    sameGroup (left, _, _) (right, _, _) = left == right

loadFixtureFile :: FilePath -> IO [(String, String, Bool)]
loadFixtureFile path = do
  content <- readFile path
  pure
    [ case map decodeEscapes parts of
        [fixtureId, sourceKind, phase, expectationKind, status, input, expected, chapterRef, notes] ->
          ( groupNameFor phase
          , fixtureId <> " [" <> sourceKind <> ", " <> status <> "] - " <> notes <> suffix chapterRef
          , runFixture expectationKind input expected
          )
        _ -> error "unreachable: filtered fixture row should have exactly 9 fields"
    | line <- lines content
    , let trimmed = dropWhile (== ' ') line
    , not (null trimmed)
    , head trimmed /= '#'
    , let parts = splitPipe line
    , length parts == 9
    ]
  where
    suffix "" = ""
    suffix ref = " (" <> ref <> ")"

    groupNameFor phase = case phase of
      "parse" -> "Fixture Parsing"
      "rename" -> "Fixture Renaming"
      "infer" -> "Fixture Inference"
      "eval" -> "Fixture Evaluation"
      "pipeline" -> "Fixture Pipeline"
      _ -> "Fixture Other"

runFixture :: String -> String -> String -> Bool
runFixture expectationKind input expected =
  case expectationKind of
    "infer-type" -> inferSource input == Right expected
    "infer-error" -> leftContains expected (inferSource input)
    "eval-value" -> runSource input == Right expected
    "pipeline-error" -> leftContains expected (runSource input)
    "parse-ok" -> parseExpectation input expected
    "parse-error" -> parseErrorExpectation input expected
    "rename-error" -> renameErrorExpectation input expected
    _ -> False

parseExpectation :: String -> String -> Bool
parseExpectation input expected =
  case parseExpr "<fixture>" input of
    Left _ -> False
    Right expr ->
      case expected of
        spanSpec | "span=" `isInfixOf` spanSpec -> renderSpan (exprSpan expr) == drop 5 spanSpec
        "literal=LitBool True" -> case expr of
          Lit _ (LitBool True) -> True
          _ -> False
        _ -> False

parseErrorExpectation :: String -> String -> Bool
parseErrorExpectation input expected =
  case parseExpr "sample.wyah" input of
    Left err -> expected `isInfixOf` show err
    Right _ -> False

renameErrorExpectation :: String -> String -> Bool
renameErrorExpectation input expected =
  case parseExpr "<fixture>" input of
    Left _ -> False
    Right expr ->
      case renameExpr expr of
        Left err -> expected `isInfixOf` show err
        Right _ -> False

splitPipe :: String -> [String]
splitPipe [] = [""]
splitPipe (char : rest)
  | char == '|' = "" : splitPipe rest
  | otherwise = case splitPipe rest of
      [] -> [[char]]
      chunk : chunks -> (char : chunk) : chunks

decodeEscapes :: String -> String
decodeEscapes [] = []
decodeEscapes ('\\' : 'n' : rest) = '\n' : decodeEscapes rest
decodeEscapes ('\\' : 't' : rest) = '\t' : decodeEscapes rest
decodeEscapes ('\\' : '\\' : rest) = '\\' : decodeEscapes rest
decodeEscapes (char : rest) = char : decodeEscapes rest

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
