module WYAH.TestCases
  ( CaseGroup(..)
  , Case(..)
  , groupedCases
  ) where

import Data.List (isInfixOf, sortOn)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import WYAH.Core
import WYAH.Desugar (desugarModule)
import WYAH.Elaborate (elaborateModule)
import WYAH.Eval
import WYAH.Infer
import WYAH.Kind
import WYAH.Parser
import WYAH.Repl
import WYAH.Renamer
import WYAH.Renamed
import WYAH.Span
import WYAH.Syntax (Expr(..), Module(..), exprSpan)
import WYAH.Type

data CaseGroup = CaseGroup
  { groupName :: String
  , cases :: [Case]
  }

data Case = Case
  { caseLabel :: String
  , caseRun :: IO (Either String ())
  }

inlineGroups :: [CaseGroup]
inlineGroups =
  [ CaseGroup "Parsing structure"
      [ testCase "addition spans the whole expression" (assertSpan "1 + 2" "1:1-1:6")
      , testCase "operator precedence uses fixities" (assertSpan "1 + 2 * 3" "1:1-1:10")
      , testCase "lambda spans binder and body" (assertSpan "\\x -> x" "1:1-1:8")
      , testCase "if spans all three parts" (assertSpan "if true then 1 else 0" "1:1-1:22")
      , testCase "prefix minus is negation" (assertSpan "- 1" "1:1-1:4")
      , testCase "let is a single expression" (assertSpan "let x = 1 in x + 2" "1:1-1:19")
      , testCase "operator applications are left nested" (assertLeftNested "1 + 2 + 3")
      , testCase "layout parses where declarations" (assertDeclCount layoutModule 2)
      , testCase "layout parses do blocks" (assertDoModule)
      , testCase "end-of-input parse errors carry a location" (assertErrorSpan "1 +" "1:4")
      ]
  , CaseGroup "Renaming and scope"
      [ testCase "shadowing does not capture outer binder" (assertEvalModule shadowModule "2")
      , testCase "let bindings are recursive" (assertEvalModule recursiveLetModule "120")
      , testCase "where bindings see function arguments" (assertEvalModule whereModule "15")
      , testCase "unbound variable is rejected with a span" (assertExprContains "phase=rename" "\\x -> y")
      , testCase "duplicate datatype declarations are rejected" (assertModuleContains "phase=rename" duplicateModule)
      ]
  , CaseGroup "Kind inference"
      [ testCase "higher-kinded parameter gets an arrow kind" (assertDataKind higherKindedModule "T1" "(* -> *) -> * -> *")
      , testCase "ill-kinded constructor is rejected" (assertModuleContains "phase=kind" illKindedModule)
      ]
  , CaseGroup "Type inference"
      [ testCase "identity is polymorphic" (assertExprType "\\x -> x" "forall t0. t0 -> t0")
      , testCase "const is polymorphic" (assertExprType "\\x -> \\y -> x" "forall t0 t1. t0 -> t1 -> t0")
      , testCase "let-polymorphism is retained" (assertExprType "let id = \\x -> x in let a = id 1 in id True" "Bool")
      , testCase "top-level functions are generalized before use" (assertProgramType polymorphicModule "main" "(Int, Bool)")
      , testCase "polymorphic recursion through map infers" (assertProgramType mapModule "main" "[Int]")
      , testCase "occurs check rejects infinite types" (assertExprContains "phase=infer" "\\x -> x x")
      , testCase "class constraints become scheme predicates" (assertProgramType classModule "same" "forall t0. Eq2 t0 => t0 -> t0 -> Bool")
      , testCase "expression annotations are checked" (assertExprType "(1 :: Int)" "Int")
      , testCase "annotations mismatch is rejected" (assertExprContains "phase=infer" "(1 :: Bool)")
      ]
  , CaseGroup "Evaluation"
      [ testCase "arithmetic and precedence" (assertModuleValue arithmeticModule "7")
      , testCase "factorial recurses" (assertModuleValue factorialModule "120")
      , testCase "algebraic datatypes destructure" (assertModuleValue dataModule "42")
      , testCase "guards choose the first true branch" (assertModuleValue guardsModule "-1")
      , testCase "records expose selectors" (assertModuleValue recordModule "\"Ada\"")
      , testCase "custom operators respect fixity" (assertModuleValue operatorModule "7")
      , testCase "list comprehensions enumerate" (assertModuleValue comprehensionModule "[4, 6]")
      , testCase "lazy infinite lists are consumed lazily" (assertModuleValue primesModule "[2, 3, 5, 7, 11]")
      , testCase "do notation uses bind and runs IO" (assertModuleValue ioModule "42")
      , testCase "class dictionaries dispatch on instance head" (assertModuleValue classModule "True")
      , testCase "superclass dictionaries are selected" (assertModuleValue superclassModule "True")
      , testCase "polymorphic instance contexts are solved" (assertModuleValue instanceContextModule "True")
      , testCase "fixpoint combinator evaluates" (assertModuleValue fixModule "120")
      , testCase "pattern guards with comma conditions" (assertModuleValue patternGuardModule "5")
      , testCase "as-patterns bind the whole value" (assertModuleValue asPatternModule "7")
      , testCase "comments are ignored by the lexer" (assertModuleValue commentModule "1")
      , testCase "show renders values" (assertModuleValue showModule "\"42\"")
      , testCase "composition and dollar sections evaluate" (assertModuleValue composeModule "7")
      , testCase "pattern annotations are checked" (assertModuleValue patternAnnotationModule "2")
      , testCase "case guards can carry where clauses" (assertModuleValue caseWhereModule "5")
      , testCase "synonyms are expanded in expression annotations" (assertModuleValue synonymAnnotationModule "\"abc\"")
      , testCase "non-exhaustive pattern matching is a runtime error" (assertModuleEvalError "pattern match failure" nonExhaustiveModule)
      ]
  ]

testCase :: String -> IO (Either String ()) -> Case
testCase label action = Case label action

assertIO :: String -> Bool -> IO (Either String ())
assertIO message condition = pure (if condition then Right () else Left message)

assertSpan :: String -> String -> IO (Either String ())
assertSpan source expected =
  case parseExpr "sample.wyah" source of
    Left err -> pure (Left (show err))
    Right expression -> assertIO ("span was " <> renderSpan (exprSpan expression)) (renderSpan (exprSpan expression) == expected)

assertLeftNested :: String -> IO (Either String ())
assertLeftNested source =
  case parseExpr "sample.wyah" source of
    Left err -> pure (Left (show err))
    Right (EApp _ (EApp _ _ _) _) -> pure (Right ())
    Right expression -> pure (Left ("expected a left-nested application, got " <> show expression))

assertDeclCount :: String -> Int -> IO (Either String ())
assertDeclCount source expected =
  case parseModule "sample.wyah" source of
    Left err -> pure (Left (show err))
    Right parsedModule -> assertIO ("declarations were " <> show (length (moduleDecls parsedModule))) (length (moduleDecls parsedModule) == expected)

assertDoModule :: IO (Either String ())
assertDoModule = assertDeclCount doModule 1

assertErrorSpan :: String -> String -> IO (Either String ())
assertErrorSpan source expected =
  case parseExpr "sample.wyah" source of
    Left err | ("span=" <> expected) `isInfixOf` show err -> pure (Right ())
    Left err -> pure (Left (show err))
    Right _ -> pure (Left "expected a parse error")

assertModuleContains :: String -> String -> IO (Either String ())
assertModuleContains needle source =
  case compileToInferred "sample.wyah" source of
    Left err -> assertIO ("error did not contain " <> needle <> ": " <> err) (needle `isInfixOf` err)
    Right _ -> pure (Left "expected the module to be rejected")

assertModuleEvalError :: String -> String -> IO (Either String ())
assertModuleEvalError needle source =
  case compileDetailed "sample.wyah" source of
    Left err -> pure (Left ("compilation failed before evaluation: " <> err))
    Right (_, _, core) ->
      case [name | (name, _) <- coreBindings core, resolvedOriginal name == "main"] of
        [] -> pure (Left "module has no main binding")
        name : _ -> do
          result <- evalWithModule "sample.wyah" core (CVar name)
          case result of
            Left err -> assertIO ("error did not contain " <> needle <> ": " <> show err) (needle `isInfixOf` show err)
            Right value -> pure (Left ("expected an evaluation error, got " <> renderValue core value))

assertExprContains :: String -> String -> IO (Either String ())
assertExprContains needle expression =
  case compileToInferred "sample.wyah" ("__repl_value = " <> expression <> "\n") of
    Left err -> assertIO ("error did not contain " <> needle <> ": " <> err) (needle `isInfixOf` err)
    Right _ -> pure (Left "expected the expression to be rejected")

assertExprType :: String -> String -> IO (Either String ())
assertExprType expression expected =
  case compileToInferred "sample.wyah" ("__repl_value = " <> expression <> "\n") of
    Left err -> pure (Left err)
    Right inferred ->
      case [prettyScheme scheme_ | (name, scheme_) <- inferredBindSchemes inferred, resolvedOriginal name == "__repl_value"] of
        scheme_ : _ -> assertIO ("type was " <> scheme_) (scheme_ == expected)
        [] -> pure (Left "missing inferred binding")

assertProgramType :: String -> String -> String -> IO (Either String ())
assertProgramType source binding expected =
  case compileToInferred "sample.wyah" source of
    Left err -> pure (Left err)
    Right inferred ->
      case [prettyScheme scheme_ | (name, scheme_) <- inferredBindSchemes inferred, resolvedOriginal name == binding] of
        scheme_ : _ -> assertIO ("type was " <> scheme_) (scheme_ == expected)
        [] -> pure (Left ("missing binding " <> binding))

assertDataKind :: String -> String -> String -> IO (Either String ())
assertDataKind source binding expected =
  case compileToInferred "sample.wyah" source of
    Left err -> pure (Left err)
    Right inferred ->
      case [prettyKind (tyConKind (dataDefTypeCon definition)) | definition <- inferredDataDefs inferred, resolvedOriginal (dataDefName definition) == binding] of
        kind : _ -> assertIO ("kind was " <> kind) (kind == expected)
        [] -> pure (Left ("missing datatype " <> binding))

assertModuleValue :: String -> String -> IO (Either String ())
assertModuleValue source expected = do
  result <- runProgram source
  case result of
    Left err -> pure (Left err)
    Right value -> assertIO ("value was " <> value) (value == expected)

assertEvalModule :: String -> String -> IO (Either String ())
assertEvalModule = assertModuleValue

runProgram :: String -> IO (Either String String)
runProgram source =
  case compileDetailed "sample.wyah" source of
    Left err -> pure (Left err)
    Right (_, _, core) ->
      case [name | (name, _) <- coreBindings core, resolvedOriginal name == "main"] of
        [] -> pure (Left "module has no main binding")
        name : _ -> do
          result <- evalWithModule "sample.wyah" core (CVar name)
          pure (either (Left . show) (Right . renderValue core) result)

compileToInferred :: FilePath -> String -> Either String InferredModule
compileToInferred file source = do
  parsed <- either (Left . show) Right (parseModule file source)
  renamed <- either (Left . show) Right (renameModule file parsed)
  let desugared = desugarModule renamed
  checked <- either (Left . show) Right (checkModule file desugared)
  either (Left . show) Right (inferModule file checked)

groupedCases :: IO [CaseGroup]
groupedCases = do
  fixtureGroups <- loadFixtureGroups
  pure (inlineGroups <> fixtureGroups)

patternAnnotationModule :: String
patternAnnotationModule = unlines
  [ "f (x :: Int) = x + 1"
  , "main = f 1"
  ]

caseWhereModule :: String
caseWhereModule = unlines
  [ "g x = case x of"
  , "  Just y | y > 0, y < 10 -> z"
  , "         | otherwise -> 0"
  , "            where z = y"
  , "  Nothing -> -1"
  , "main = g (Just 5)"
  ]

synonymAnnotationModule :: String
synonymAnnotationModule = unlines
  [ "type String2 = [Char]"
  , "main = (\"abc\" :: String2)"
  ]

fixModule :: String
fixModule = unlines
  [ "fixfact = fix (\\fact -> \\n -> if n == 0 then 1 else n * fact (n - 1))"
  , "main = fixfact 5"
  ]

patternGuardModule :: String
patternGuardModule = unlines
  [ "pat x"
  , "  | Just y <- x, y > 0 = y"
  , "  | otherwise = 0"
  , "main = pat (Just 5)"
  ]

asPatternModule :: String
asPatternModule = unlines
  [ "f p@(Just x) = x"
  , "main = f (Just 7)"
  ]

commentModule :: String
commentModule = unlines
  [ "main = 1 -- trailing comment"
  ]

showModule :: String
showModule = unlines
  [ "main = show 42"
  ]

composeModule :: String
composeModule = unlines
  [ "addOne = (\\x -> x + 1) . (\\y -> y * 2)"
  , "main = addOne 3"
  ]

-- Fixtures ------------------------------------------------------------------

loadFixtureGroups :: IO [CaseGroup]
loadFixtureGroups = do
  root <- locateFixtureRoot
  rows <- concat <$> mapM (loadFixtureFile root) fixtureFiles
  let sortedRows = sortOn (\(group, _, _) -> group) rows
  pure
    [ CaseGroup group [testCase label result | (_, label, result) <- grouped]
    | grouped@((group, _, _) : _) <- groupBySameGroup sortedRows
    ]
  where
    fixtureFiles =
      [ "parser.tsv"
      , "rename.tsv"
      , "infer.tsv"
      , "eval.tsv"
      , "program.tsv"
      , "classes.tsv"
      ]
    groupBySameGroup [] = []
    groupBySameGroup (first : rest) =
      let (same, different) = span (\(group, _, _) -> group == fst3 first) rest
       in (first : same) : groupBySameGroup different
    fst3 (a, _, _) = a

locateFixtureRoot :: IO FilePath
locateFixtureRoot = go candidates
  where
    candidates =
      [ "test" </> "fixtures"
      , ".." </> "test" </> "fixtures"
      , ".." </> ".." </> "test" </> "fixtures"
      ]
    go [] = ioError (userError "could not locate test/fixtures")
    go (candidate : rest) = do
      exists <- doesFileExist (candidate </> "parser.tsv")
      if exists then pure candidate else go rest

loadFixtureFile :: FilePath -> FilePath -> IO [(String, String, IO (Either String ()))]
loadFixtureFile root name = do
  let path = root </> name
  content <- readFile path
  let parsed = [parseRow (lines content)]
  pure (concat parsed)
  where
    parseRow rows = concatMap parseLine (zip [1 ..] rows)
    parseLine (lineNumber, line)
      | null trimmed = []
      | head trimmed == '#' = []
      | otherwise =
          case map decodeEscapes (splitPipe line) of
            [fixtureId, sourceKind, phase, expectationKind, status, input, expected, chapterRef, notes]
              | length (splitPipe line) == 9 ->
                  case makeFixture fixtureId sourceKind phase expectationKind status input expected chapterRef notes of
                    Just entry -> [entry]
                    Nothing -> []
            _ -> error ("malformed fixture at line " <> show lineNumber)
      where
        trimmed = dropWhile (== ' ') line

    makeFixture fixtureId sourceKind phase expectationKind status input expected chapterRef notes
      | status /= "active-now" = Nothing
      | otherwise = Just (groupFor phase, label, runFixture sourceKind expectationKind input expected)
      where
        label =
          fixtureId
            <> " ["
            <> sourceKind
            <> ", "
            <> status
            <> "] - "
            <> notes
            <> (if null chapterRef then "" else " (" <> chapterRef <> ")")

    groupFor phase = case phase of
      "parse" -> "Fixture Parsing"
      "rename" -> "Fixture Renaming"
      "kind" -> "Fixture Kind"
      "infer" -> "Fixture Inference"
      "eval" -> "Fixture Evaluation"
      "program" -> "Fixture Programs"
      _ -> "Fixture Pipeline"

runFixture :: String -> String -> String -> String -> IO (Either String ())
runFixture sourceKind expectationKind input expected = case expectationKind of
  "parse-ok" -> pure (checkParseOk sourceKind input expected)
  "parse-error" -> pure (checkParseError sourceKind input expected)
  "parse-span" ->
    pure
      ( case parseExpressionFor sourceKind input of
          Left err -> Left err
          Right (ParsedExpression expression) -> assertExpected ("span was " <> renderSpan (exprSpan expression)) expected ("span=" <> renderSpan (exprSpan expression))
          Right (ParsedProgram _) -> Left "parse-span is only defined for expressions"
      )
  "rename-error" -> pure (checkCompileContains "phase=rename" (wrapExpression sourceKind input) expected)
  "infer-error" -> pure (checkCompileContains "phase=infer" (wrapExpression sourceKind input) expected)
  "kind-error" -> pure (checkCompileContains "phase=kind" (wrapExpression sourceKind input) expected)
  "infer-type" ->
    pure
      ( case compileToInferred "fixture.wyah" (wrapExpression sourceKind input) of
          Left err -> Left err
          Right inferred ->
            case [prettyScheme scheme_ | (name, scheme_) <- inferredBindSchemes inferred, resolvedOriginal name == targetName sourceKind] of
              scheme_ : _ -> assertExpected ("type was " <> scheme_) expected scheme_
              [] -> Left "missing inferred binding"
      )
  "eval-value" -> do
    result <- runFixtureValue sourceKind input
    pure (either Left (\value -> assertExpected ("value was " <> value) expected value) result)
  "eval-error" -> do
    result <- runFixtureValue sourceKind input
    pure
      ( case result of
          Left err -> assert ("error did not contain " <> expected <> ": " <> err) (expected `isInfixOf` err)
          Right value -> Left ("expected an evaluation error, got " <> value)
      )
  "program-type" -> pure (checkProgramType input expected)
  "program-value" -> do
    result <- runProgram input
    pure (either Left (\value -> assertExpected ("value was " <> value) expected value) result)
  _ -> pure (Left ("unknown expectation kind " <> expectationKind))

targetName :: String -> String
targetName sourceKind = case sourceKind of
  "program" -> "main"
  _ -> "__repl_value"

wrapExpression :: String -> String -> String
wrapExpression sourceKind input = case sourceKind of
  "program" -> input
  _ -> "__repl_value = " <> input <> "\n"

data ParsedInput = ParsedExpression Expr | ParsedProgram Module

parseExpressionFor :: String -> String -> Either String ParsedInput
parseExpressionFor sourceKind input = case sourceKind of
  "expr" -> ParsedExpression <$> either (Left . show) Right (parseExpr "fixture.wyah" input)
  "program" -> ParsedProgram <$> either (Left . show) Right (parseModule "fixture.wyah" input)
  _ -> Left ("unknown source kind " <> sourceKind)

checkParseOk :: String -> String -> String -> Either String ()
checkParseOk sourceKind input expected =
  case parseExpressionFor sourceKind input of
    Right (ParsedProgram _) -> Right ()
    Right (ParsedExpression expression) ->
      if expected == "ok" || ("span=" <> renderSpan (exprSpan expression)) == expected
        then Right ()
        else Left ("span was " <> renderSpan (exprSpan expression))
    Left err -> Left err

checkParseError :: String -> String -> String -> Either String ()
checkParseError sourceKind input expected =
  case parseExpressionFor sourceKind input of
    Left err -> assert ("error did not contain " <> expected <> ": " <> err) (expected `isInfixOf` err)
    Right _ -> Left "expected a parse error"

checkCompileContains :: String -> String -> String -> Either String ()
checkCompileContains phase input expected =
  case compileToInferred "fixture.wyah" input of
    Left err -> assert ("error did not contain " <> phase <> ": " <> err) (phase `isInfixOf` err && expected `isInfixOf` err)
    Right _ -> Left "expected the program to be rejected"

checkProgramType :: String -> String -> Either String ()
checkProgramType input expected =
  case compileToInferred "fixture.wyah" input of
    Left err -> Left err
    Right inferred ->
      case [prettyScheme scheme_ | (name, scheme_) <- inferredBindSchemes inferred, resolvedOriginal name == "main"] of
        scheme_ : _ -> assertExpected ("type was " <> scheme_) expected scheme_
        [] -> Left "missing main binding"

runFixtureValue :: String -> String -> IO (Either String String)
runFixtureValue sourceKind input = case sourceKind of
  "program" -> runProgram input
  _ -> do
    result <- runSource input
    pure $ case result of
      Left err -> Left err
      Right runResult -> maybe (Left "no value produced") Right (resultValue runResult)

assert :: String -> Bool -> Either String ()
assert message condition = if condition then Right () else Left message

assertExpected :: String -> String -> String -> Either String ()
assertExpected message expected actual = assert (message <> ", expected " <> show expected) (actual == expected)

-- Escaping and splitting ----------------------------------------------------

splitPipe :: String -> [String]
splitPipe = go []
  where
    go current [] = [reverse current]
    go current ('\\' : '|' : rest) = go ('|' : current) rest
    go current ('|' : rest) = reverse current : go [] rest
    go current (char : rest) = go (char : current) rest

decodeEscapes :: String -> String
decodeEscapes [] = []
decodeEscapes ('\\' : 'n' : rest) = '\n' : decodeEscapes rest
decodeEscapes ('\\' : 't' : rest) = '\t' : decodeEscapes rest
decodeEscapes ('\\' : '\\' : rest) = '\\' : decodeEscapes rest
decodeEscapes (char : rest) = char : decodeEscapes rest

-- Inline fixture sources ----------------------------------------------------

layoutModule :: String
layoutModule = unlines
  [ "f x = y"
  , "  where"
  , "    y = x + 1"
  , "g = 2"
  ]

doModule :: String
doModule = unlines
  [ "main = do"
  , "  putStrLn \"hello\""
  ]

shadowModule :: String
shadowModule = unlines
  [ "f x = (\\x -> x) 2"
  , "main = f 1"
  ]

recursiveLetModule :: String
recursiveLetModule = unlines
  [ "main = let fact n = if n == 0 then 1 else n * fact (n - 1) in fact 5"
  ]

whereModule :: String
whereModule = unlines
  [ "f x = go 0 x"
  , "  where"
  , "    go acc n = if n == 0 then acc else go (acc + n) (n - 1)"
  , "main = f 5"
  ]

duplicateModule :: String
duplicateModule = unlines
  [ "data Dup = A"
  , "data Dup = B"
  , "main = 1"
  ]

higherKindedModule :: String
higherKindedModule = unlines
  [ "data T1 f a = T1 (f a)"
  ]

illKindedModule :: String
illKindedModule = unlines
  [ "data Bad = Bad (Maybe Int Int)"
  ]

polymorphicModule :: String
polymorphicModule = unlines
  [ "id x = x"
  , "main = (id 1, id True)"
  ]

mapModule :: String
mapModule = unlines
  [ "map f [] = []"
  , "map f (x:xs) = f x : map f xs"
  , "main = map (\\x -> x + 1) [1,2,3]"
  ]

classModule :: String
classModule = unlines
  [ "class Eq2 a where"
  , "  eq2 :: a -> a -> Bool"
  , "instance Eq2 Int where"
  , "  eq2 x y = x == y"
  , "same :: Eq2 a => a -> a -> Bool"
  , "same x y = eq2 x y"
  , "main = same 1 1"
  ]

superclassModule :: String
superclassModule = unlines
  [ "class Eq2 a where"
  , "  eq2 :: a -> a -> Bool"
  , "class Eq2 a => Ord2 a where"
  , "  cmp2 :: a -> a -> Bool"
  , "instance Eq2 Int where"
  , "  eq2 x y = x == y"
  , "instance Ord2 Int where"
  , "  cmp2 x y = x < y"
  , "f :: Ord2 a => a -> a -> Bool"
  , "f x y = eq2 x y"
  , "main = f 1 1"
  ]

instanceContextModule :: String
instanceContextModule = unlines
  [ "class Eq2 a where"
  , "  eq2 :: a -> a -> Bool"
  , "instance Eq2 Int where"
  , "  eq2 x y = x == y"
  , "instance Eq2 a => Eq2 [a] where"
  , "  eq2 xs ys = case xs of"
  , "    [] -> case ys of"
  , "            [] -> True"
  , "            _ -> False"
  , "    (x:xs') -> case ys of"
  , "                 [] -> False"
  , "                 (y:ys') -> if eq2 x y then eq2 xs' ys' else False"
  , "same :: Eq2 a => a -> a -> Bool"
  , "same x y = eq2 x y"
  , "main = same [1,2] [1,2]"
  ]

arithmeticModule :: String
arithmeticModule = "main = 1 + 2 * 3\n"

factorialModule :: String
factorialModule = unlines
  [ "fact 0 = 1"
  , "fact n = n * fact (n - 1)"
  , "main = fact 5"
  ]

dataModule :: String
dataModule = unlines
  [ "data Maybe2 a = Nothing2 | Just2 a"
  , "fromMaybe d Nothing2 = d"
  , "fromMaybe d (Just2 x) = x"
  , "main = fromMaybe 0 (Just2 42)"
  ]

guardsModule :: String
guardsModule = unlines
  [ "sign x"
  , "  | x > 0 = 1"
  , "  | x < 0 = -1"
  , "  | otherwise = 0"
  , "main = sign (-5)"
  ]

recordModule :: String
recordModule = unlines
  [ "data Person = Person { name :: [Char], age :: Int }"
  , "main = name (Person \"Ada\" 36)"
  ]

operatorModule :: String
operatorModule = unlines
  [ "infixl 6 <+>"
  , "(<+>) :: Int -> Int -> Int"
  , "x <+> y = x + y"
  , "main = 1 <+> 2 * 3"
  ]

comprehensionModule :: String
comprehensionModule = unlines
  [ "main = [x * 2 | x <- [1,2,3], x > 1]"
  ]

primesModule :: String
primesModule = unlines
  [ "sieve [] = []"
  , "sieve (p:ps) = p : sieve (filter (nonMultiple p) ps)"
  , "nonMultiple p n = ((n / p) * p) /= n"
  , "primes = sieve [2..]"
  , "main = take 5 primes"
  ]

ioModule :: String
ioModule = unlines
  [ "main = do"
  , "  x <- return 41"
  , "  return (x + 1)"
  ]

nonExhaustiveModule :: String
nonExhaustiveModule = unlines
  [ "f (Just x) = x"
  , "main = f Nothing"
  ]
