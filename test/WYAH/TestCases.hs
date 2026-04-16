module WYAH.TestCases
  ( CaseGroup(..)
  , Case(..)
  , groupedCases
  ) where

import Data.List (groupBy, isInfixOf, sortOn)
import WYAH.Eval (EvalError(..))
import WYAH.Infer (inferExpr, inferProgram)
import WYAH.Name (RenameError, RenamedExpr, RenamedProgram, renameExpr, renameProgram)
import WYAH.Parser (parseExpr, parseProgram)
import WYAH.Repl (runSource)
import WYAH.Span (renderSpan)
import WYAH.Syntax (Decl(..), Expr(..), Literal(..), Program(..), exprSpan)
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
  , CaseGroup "Diagnostics"
      [ AssertionCase "parse diagnostics are normalized" (diagnosticHas ["phase=parse","source=sample.wyah","span=1:1-1:2"] showParseError)
      , AssertionCase "rename diagnostics are normalized" (diagnosticHas ["phase=rename","source=<unknown>","span=1:7-1:8"] showRenameError)
      , AssertionCase "infer diagnostics are normalized" (diagnosticHas ["phase=infer","source=<unknown>","span=1:1-1:19"] showInferError)
      , AssertionCase "eval diagnostics are normalized" (diagnosticHas ["phase=eval","source=<unknown>","span=<unknown>"] (show (EvalError "boom")))
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
      , "test/fixtures/program.tsv"
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
          , runFixture sourceKind expectationKind input expected
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

runFixture :: String -> String -> String -> String -> Bool
runFixture sourceKind expectationKind input expected =
  case expectationKind of
    "infer-type" -> inferSource sourceKind input == Right expected
    "infer-error" -> leftContains expected (inferSource sourceKind input)
    "eval-value" -> runSource input == Right expected
    "pipeline-error" -> leftContains expected (runSource input)
    "parse-ok" -> parseExpectation sourceKind input expected
    "parse-error" -> parseErrorExpectation sourceKind input expected
    "rename-error" -> renameErrorExpectation sourceKind input expected
    _ -> False

parseExpectation :: String -> String -> String -> Bool
parseExpectation sourceKind input expected =
  case parseInput sourceKind input of
    Left _ -> False
    Right parsed ->
      case (sourceKind, expected, parsed) of
        ("expr", spanSpec, Left expr) | "span=" `isInfixOf` spanSpec -> renderSpan (exprSpan expr) == drop 5 spanSpec
        ("expr", "literal=LitBool True", Left (Lit _ (LitBool True))) -> True
        ("program", declSpec, Right program) | "decls=" `isInfixOf` declSpec ->
          case reads (drop 6 declSpec) of
            [(n, "")] -> length (programDecls program) == n
            _ -> False
        _ -> False

parseErrorExpectation :: String -> String -> String -> Bool
parseErrorExpectation sourceKind input expected =
  case parseInput sourceKind input of
    Left err -> leftContains expected (Left (show err))
    Right _ -> False

renameErrorExpectation :: String -> String -> String -> Bool
renameErrorExpectation sourceKind input expected =
  case parseInput sourceKind input of
    Left _ -> False
    Right parsed ->
      case renameParsed parsed of
        Left err -> leftContains expected (Left (show err))
        Right _ -> False

parseInput :: String -> String -> Either String (Either Expr Program)
parseInput sourceKind input =
  case sourceKind of
    "expr" -> either (Left . show) (Right . Left) (parseExpr parseFile input)
    "program" -> either (Left . show) (Right . Right) (parseProgram parseFile input)
    _ -> Left ("unsupported sourceKind: " <> sourceKind)
  where
    parseFile = "sample.wyah"

programDecls :: Program -> [Decl]
programDecls (Program decls _) = decls

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
parseErrorHasLocation = diagnosticHas ["phase=parse","source=sample.wyah","span=1:1-1:2"] showParseError

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

showParseError :: String
showParseError = case parseExpr "sample.wyah" "(" of
  Left err -> show err
  Right _ -> ""

showRenameError :: String
showRenameError = case parseExpr "<test>" "\\x -> y" of
  Left _ -> ""
  Right expr -> case renameExpr expr of
    Left err -> show err
    Right _ -> ""

showInferError :: String
showInferError = case inferSource "expr" "if 1 then 2 else 3" of
  Left err -> err
  Right _ -> ""

diagnosticHas :: [String] -> String -> Bool
diagnosticHas needles haystack = all (`isInfixOf` haystack) needles

renameErrorHasLocation :: Bool
renameErrorHasLocation = diagnosticHas ["phase=rename","span=1:7-1:8"] showRenameError

renameParsed :: Either Expr Program -> Either RenameError (Either RenamedExpr RenamedProgram)
renameParsed parsed = case parsed of
  Left expr -> Left <$> renameExpr expr
  Right program -> Right <$> renameProgram program

inferSource :: String -> String -> Either String String
inferSource sourceKind input = do
  parsed <- parseInput sourceKind input
  renamed <- eitherShow (renameParsed parsed)
  case renamed of
    Left renamedExpr -> prettyType . normalizeTypeVariables <$> eitherShow (inferExpr renamedExpr)
    Right renamedProgram -> prettyType . normalizeTypeVariables <$> eitherShow (inferProgram renamedProgram)

leftContains :: String -> Either String String -> Bool
leftContains needle result =
  case result of
    Left message -> needle `isInfixOf` message
    Right _ -> False

eitherShow :: Show e => Either e a -> Either String a
eitherShow = either (Left . show) Right
