-- | Call-by-need interpreter for Core.
module WYAH.Eval
  ( Value(..)
  , EvalError(..)
  , evalWithModule
  , evalPure
  , showValue
  , showValueIn
  , valueToString
  , renderValue
  ) where

import WYAH.Core
import WYAH.Diagnostic
import WYAH.Prelude (builtinName)
import WYAH.Renamed
import WYAH.Span (Span(..))
import WYAH.Syntax (Literal(..))

data Thunk
  = Thunk Env CoreExpr
  | ThunkValue Value
  | ThunkDelay (Either EvalError Value)

type Env = [(ResolvedName, Thunk)]

data Value
  = VInt Integer
  | VBool Bool
  | VChar Char
  | VUnit
  | VCon ResolvedName [Thunk]
  | VClosure Env [ResolvedName] CoreExpr
  | VPrimitive String [Thunk]
  | VConFun ResolvedName [Thunk]
  | VDict String [(Selector, Thunk)]
  | VIO (IO (Either EvalError Value))

instance Show Value where
  show = showValue

data EvalError = EvalError
  { evalErrorSource :: FilePath
  , evalErrorMessage :: String
  }
  deriving (Eq)

instance Show EvalError where
  show err =
    renderDiagnostic
      Diagnostic
        { diagnosticPhase = phaseName EvalPhase
        , diagnosticSource = Just (evalErrorSource err)
        , diagnosticSpan = Just NoSpan
        , diagnosticMessage = evalErrorMessage err
        }

data EvalContext = EvalContext
  { contextConstructors :: [(ResolvedName, Int)]
  , contextSource :: FilePath
  }

evalWithModule :: FilePath -> CoreModule -> CoreExpr -> IO (Either EvalError Value)
evalWithModule source core expression = do
  let context = EvalContext (coreConstructors core) source
      environment = moduleEnvironment context core
  case eval context environment expression of
    Left err -> pure (Left err)
    Right value -> runIOValue value

evalPure :: FilePath -> CoreModule -> CoreExpr -> Either EvalError Value
evalPure source core expression =
  let context = EvalContext (coreConstructors core) source
      environment = moduleEnvironment context core
   in eval context environment expression

moduleEnvironment :: EvalContext -> CoreModule -> Env
moduleEnvironment context core = bindings <> preludeEnvironment context
  where
    bindings = [(name, Thunk environment expression) | (name, expression) <- coreBindings core]
    environment = bindings <> preludeEnvironment context

preludeEnvironment :: EvalContext -> Env
preludeEnvironment context =
  [ (builtinName name, ThunkValue (VPrimitive name []))
  | name <- preludePrimitiveNames
  ]
    <> [ (builtinName "otherwise", ThunkValue (VBool True))
       , (builtinName "undefined", ThunkDelay (Left (evalError context "undefined")))
       ]

preludePrimitiveNames :: [String]
preludePrimitiveNames =
  [ "id", "const", "compose", "flip", "not", "map", "filter", "foldr", "foldl"
  , "head", "tail", "null", "length", "++", "take", "drop", "reverse", "concat"
  , "concatMap", "zip", "fst", "snd", "error", "show", "succ", "pred", "iszero"
  , "add", "negate", "div", "mod", "quot", "rem", "even", "odd", "enumFrom"
  , "enumFromThen", "enumFromTo", "enumFromThenTo", "seq", "fix", "+", "-", "*", "/"
  , "==", "/=", "<", "<=", ">", ">=", "&&", "||", ".", "$", "print", "putStr"
  , "putStrLn", "getLine", "return", ">>=", ">>", "fail"
  ]

eval :: EvalContext -> Env -> CoreExpr -> Either EvalError Value
eval context environment expression = case expression of
  CVar name ->
    case lookup name environment of
      Just thunk -> force context thunk
      Nothing -> Left (evalError context ("unbound Core variable " <> show name))
  CLit (LInt value) -> Right (VInt value)
  CLit (LBool value) -> Right (VBool value)
  CLit (LChar value) -> Right (VChar value)
  CLit (LString value) -> Right (stringValue value)
  CLam [] body -> eval context environment body
  CLam names body -> Right (VClosure environment names body)
  CApp function argument -> do
    functionValue <- eval context environment function
    apply context functionValue (Thunk environment argument)
  CLet bindings body ->
    let environment' = [(name, Thunk environment value) | (name, value) <- bindings] <> environment
     in eval context environment' body
  CLetRec bindings body ->
    let environment' = [(name, Thunk environment' value) | (name, value) <- bindings] <> environment
     in eval context environment' body
  CCase scrutinee alternatives fallback -> do
    scrutineeValue <- eval context environment scrutinee
    matchAlternatives context environment scrutineeValue alternatives fallback
  CCon name
    | name == builtinName "True" -> Right (VBool True)
    | name == builtinName "False" -> Right (VBool False)
  CCon name ->
    case lookup name (contextConstructors context) of
      Just 0 -> Right (VCon name [])
      Just _ -> Right (VConFun name [])
      Nothing -> Left (evalError context ("unknown constructor " <> show name))
  CDict className fields ->
    Right (VDict className [(selector, Thunk environment expression) | (selector, expression) <- fields])
  CSelect dictionary selector -> do
    dictionaryValue <- eval context environment dictionary
    case dictionaryValue of
      VDict _ fields ->
        case lookup selector fields of
          Just thunk -> force context thunk
          Nothing -> Left (evalError context ("missing dictionary field " <> show selector))
      _ -> Left (evalError context "selection from a non-dictionary value")
  CError message -> Left (evalError context message)

force :: EvalContext -> Thunk -> Either EvalError Value
force context thunk = case thunk of
  ThunkValue value -> Right value
  Thunk environment expression -> eval context environment expression
  ThunkDelay result -> result

apply :: EvalContext -> Value -> Thunk -> Either EvalError Value
apply context functionValue argument = case functionValue of
  VClosure environment names body -> case names of
    [] -> eval context environment body
    name : rest -> do
      argumentValue <- force context argument
      let environment' = (name, ThunkValue argumentValue) : environment
      case rest of
        [] -> eval context environment' body
        _ -> Right (VClosure environment' rest body)
  VPrimitive name arguments -> applyPrimitive context name (arguments <> [argument])
  VConFun name arguments ->
    case lookup name (contextConstructors context) of
      Nothing -> Left (evalError context ("unknown constructor " <> show name))
      Just arity ->
        let arguments' = arguments <> [argument]
         in if length arguments' < arity
              then Right (VConFun name arguments')
              else
                if length arguments' == arity
                  then Right (VCon name arguments')
                  else Left (evalError context ("constructor " <> show name <> " applied to too many arguments"))
  _ -> Left (evalError context "attempted to apply a non-function value")

matchAlternatives :: EvalContext -> Env -> Value -> [CoreAlt] -> CoreExpr -> Either EvalError Value
matchAlternatives context environment scrutinee alternatives fallback = go alternatives
  where
    go [] = eval context environment fallback
    go (CoreAlt pattern_ body : rest) =
      case matchValue context pattern_ scrutinee of
        Nothing -> go rest
        Just bindings -> eval context (bindings <> environment) body

matchValue :: EvalContext -> CorePat -> Value -> Maybe [(ResolvedName, Thunk)]
matchValue context pattern_ value = case pattern_ of
  CPVar name -> Just [(name, ThunkValue value)]
  CPWild -> Just []
  CPLit literal -> if literalMatches literal value then Just [] else Nothing
  CPCon name []
    | name == builtinName "True" -> case value of VBool True -> Just []; _ -> Nothing
    | name == builtinName "False" -> case value of VBool False -> Just []; _ -> Nothing
  CPCon name patterns ->
    case value of
      VCon constructorName fields
        | constructorName == name && length fields == length patterns ->
            matchFields context patterns fields
      _ -> Nothing
  CPAs name inner -> do
    bindings <- matchValue context inner value
    pure ((name, ThunkValue value) : bindings)

matchFields :: EvalContext -> [CorePat] -> [Thunk] -> Maybe [(ResolvedName, Thunk)]
matchFields _ [] [] = Just []
matchFields context (pattern_ : patterns) (thunk : thunks) = do
  bindings <- matchThunk context pattern_ thunk
  rest <- matchFields context patterns thunks
  pure (bindings <> rest)
matchFields _ _ _ = Nothing

matchThunk :: EvalContext -> CorePat -> Thunk -> Maybe [(ResolvedName, Thunk)]
matchThunk context pattern_ thunk = case pattern_ of
  CPVar name -> Just [(name, thunk)]
  CPWild -> Just []
  _ -> case force context thunk of
    Left _ -> Nothing
    Right value -> matchValue context pattern_ value

literalMatches :: Literal -> Value -> Bool
literalMatches literal value = case (literal, value) of
  (LInt left, VInt right) -> left == right
  (LBool left, VBool right) -> left == right
  (LChar left, VChar right) -> left == right
  _ -> False

-- Primitive dispatch --------------------------------------------------------

applyPrimitive :: EvalContext -> String -> [Thunk] -> Either EvalError Value
applyPrimitive context name arguments
  | length arguments < primitiveArity name = pure (VPrimitive name arguments)
  | otherwise = dispatch context name arguments

primitiveArity :: String -> Int
primitiveArity name = case name of
  "id" -> 1; "const" -> 2; "compose" -> 3; "." -> 3; "flip" -> 3; "$" -> 2
  "not" -> 1; "map" -> 2; "filter" -> 2; "foldr" -> 3; "foldl" -> 3
  "head" -> 1; "tail" -> 1; "null" -> 1; "length" -> 1; "++" -> 2
  "take" -> 2; "drop" -> 2; "reverse" -> 1; "concat" -> 1; "concatMap" -> 2
  "zip" -> 2; "fst" -> 1; "snd" -> 1; "error" -> 1; "show" -> 1
  "succ" -> 1; "pred" -> 1; "iszero" -> 1; "add" -> 2; "negate" -> 1
  "div" -> 2; "mod" -> 2; "quot" -> 2; "rem" -> 2; "even" -> 1; "odd" -> 1
  "enumFrom" -> 1; "enumFromThen" -> 2; "enumFromTo" -> 2; "enumFromThenTo" -> 3
  "seq" -> 2; "fix" -> 1; "+" -> 2; "-" -> 2; "*" -> 2; "/" -> 2
  "==" -> 2; "/=" -> 2; "<" -> 2; "<=" -> 2; ">" -> 2; ">=" -> 2
  "&&" -> 2; "||" -> 2; "print" -> 1; "putStr" -> 1; "putStrLn" -> 1
  "getLine" -> 0; "return" -> 1; ">>=" -> 2; ">>" -> 2; "fail" -> 1
  _ -> 0

dispatch :: EvalContext -> String -> [Thunk] -> Either EvalError Value
dispatch context name arguments = case name of
  "id" -> force context (arguments !! 0)
  "const" -> force context (arguments !! 0)
  "compose" -> composePrimitive
  "." -> composePrimitive
  "flip" -> do
    function <- force context (arguments !! 0)
    second <- force context (arguments !! 1)
    third <- force context (arguments !! 2)
    partially <- apply context function (ThunkValue third)
    apply context partially (ThunkValue second)
  "$" -> do
    function <- force context (arguments !! 0)
    apply context function (arguments !! 1)
  "not" -> unaryBool not
  "map" -> do
    function <- force context (arguments !! 0)
    pure (mapList context function (arguments !! 1))
  "filter" -> do
    function <- force context (arguments !! 0)
    pure (filterList context function (arguments !! 1))
  "foldr" -> do
    function <- force context (arguments !! 0)
    pure (foldrList context function (arguments !! 1) (arguments !! 2))
  "foldl" -> do
    function <- force context (arguments !! 0)
    initial <- force context (arguments !! 1)
    pure (foldlList context function initial (arguments !! 2))
  "head" -> case forceList context (arguments !! 0) of
    Left err -> Left err
    Right Nothing -> Left (evalError context "head of empty list")
    Right (Just (headThunk, _)) -> force context headThunk
  "tail" -> case forceList context (arguments !! 0) of
    Left err -> Left err
    Right Nothing -> Left (evalError context "tail of empty list")
    Right (Just (_, tailThunk)) -> pure (VCon (builtinName ":") [tailThunk])
  "null" -> case forceList context (arguments !! 0) of
    Left err -> Left err
    Right Nothing -> Right (VBool True)
    Right (Just _) -> Right (VBool False)
  "length" -> do
    count <- lengthList context (arguments !! 0)
    pure (VInt (fromIntegral count))
  "++" -> do
    left <- force context (arguments !! 0)
    pure (appendLists context (ThunkValue left) (arguments !! 1))
  "take" -> case force context (arguments !! 0) of
    Left err -> Left err
    Right (VInt count) -> pure (takeList context (fromIntegral count) (arguments !! 1))
    _ -> Left (evalError context "take expects an Int")
  "drop" -> case force context (arguments !! 0) of
    Left err -> Left err
    Right (VInt count) -> pure (dropList context (fromIntegral count) (arguments !! 1))
    _ -> Left (evalError context "drop expects an Int")
  "reverse" -> pure (reverseList context (arguments !! 0))
  "concat" -> pure (concatLists context (arguments !! 0))
  "concatMap" -> do
    function <- force context (arguments !! 0)
    pure (concatMapList context function (arguments !! 1))
  "zip" -> pure (zipLists context (arguments !! 0) (arguments !! 1))
  "fst" -> pairFirst
  "snd" -> pairSecond
  "error" -> do
    message <- expectStringValue context (arguments !! 0)
    Left (evalError context message)
  "show" -> do
    value <- force context (arguments !! 0)
    pure (stringValue (showValueIn context value))
  "succ" -> numericUnary (+ 1)
  "pred" -> numericUnary (subtract 1)
  "negate" -> numericUnary negate
  "iszero" -> do
    value <- force context (arguments !! 0)
    case value of
      VInt number -> pure (VBool (number == 0))
      _ -> Left (evalError context "iszero expects an Int")
  "even" -> numericPredicate even
  "odd" -> numericPredicate odd
  "add" -> numericBinary (+)
  "+" -> numericBinary (+)
  "-" -> numericBinary (-)
  "*" -> numericBinary (*)
  "/" -> numericDivision quot
  "div" -> numericDivision div
  "mod" -> numericDivision mod
  "quot" -> numericDivision quot
  "rem" -> numericDivision rem
  "==" -> comparison (==)
  "/=" -> comparison (/=)
  "<" -> comparison (<)
  "<=" -> comparison (<=)
  ">" -> comparison (>)
  ">=" -> comparison (>=)
  "&&" -> shortCircuitAnd
  "||" -> shortCircuitOr
  "enumFrom" -> do
    start <- intArgument 0
    pure (enumFromList start)
  "enumFromThen" -> do
    start <- intArgument 0
    step <- intArgument 1
    pure (enumFromThenList start step)
  "enumFromTo" -> do
    start <- intArgument 0
    end <- intArgument 1
    pure (enumFromToList start end)
  "enumFromThenTo" -> do
    start <- intArgument 0
    step <- intArgument 1
    end <- intArgument 2
    pure (enumFromThenToList start step end)
  "seq" -> do
    _ <- force context (arguments !! 0)
    force context (arguments !! 1)
  "fix" -> do
    function <- force context (arguments !! 0)
    pure (fixValue context function)
  "return" -> pure (VIO (pure (force context (arguments !! 0))))
  ">>=" -> do
    action <- force context (arguments !! 0)
    function <- force context (arguments !! 1)
    pure (bindIO context action function)
  ">>" -> do
    first <- force context (arguments !! 0)
    second <- force context (arguments !! 1)
    pure (thenIO context first second)
  "fail" -> do
    message <- expectStringValue context (arguments !! 0)
    pure (VIO (pure (Left (evalError context message))))
  "putStr" -> putPrimitive False
  "putStrLn" -> putPrimitive True
  "print" -> do
    value <- force context (arguments !! 0)
    pure (VIO (do putStrLn (showValueIn context value); pure (Right VUnit)))
  "getLine" -> pure (VIO (Right . stringValue <$> getLine))
  _ -> Left (evalError context ("unknown primitive " <> name))
  where
    composePrimitive = do
      function <- force context (arguments !! 0)
      inner <- force context (arguments !! 1)
      argument <- force context (arguments !! 2)
      intermediate <- apply context inner (ThunkValue argument)
      apply context function (ThunkValue intermediate)
    unaryBool operation = do
      value <- force context (arguments !! 0)
      case value of
        VBool boolean -> pure (VBool (operation boolean))
        _ -> Left (evalError context (name <> " expects a Bool"))
    numericUnary operation = do
      value <- force context (arguments !! 0)
      case value of
        VInt number -> pure (VInt (operation number))
        _ -> Left (evalError context (name <> " expects an Int"))
    numericPredicate predicate = do
      value <- force context (arguments !! 0)
      case value of
        VInt number -> pure (VBool (predicate number))
        _ -> Left (evalError context (name <> " expects an Int"))
    numericBinary operation = do
      left <- force context (arguments !! 0)
      right <- force context (arguments !! 1)
      case (left, right) of
        (VInt a, VInt b) -> pure (VInt (operation a b))
        _ -> Left (evalError context (name <> " expects Int arguments"))
    numericDivision operation = do
      left <- force context (arguments !! 0)
      right <- force context (arguments !! 1)
      case (left, right) of
        (VInt a, VInt b)
          | b == 0 -> Left (evalError context "division by zero")
          | otherwise -> pure (VInt (operation a b))
        _ -> Left (evalError context (name <> " expects Int arguments"))
    comparison operation = do
      left <- force context (arguments !! 0)
      right <- force context (arguments !! 1)
      case (left, right) of
        (VInt a, VInt b) -> pure (VBool (operation a b))
        _ -> Left (evalError context (name <> " expects Int arguments"))
    booleanBinary operation = do
      left <- force context (arguments !! 0)
      right <- force context (arguments !! 1)
      case (left, right) of
        (VBool a, VBool b) -> pure (VBool (operation a b))
        _ -> Left (evalError context (name <> " expects Bool arguments"))
    shortCircuitAnd = do
      left <- force context (arguments !! 0)
      case left of
        VBool False -> pure (VBool False)
        VBool True -> do
          right <- force context (arguments !! 1)
          case right of
            VBool value -> pure (VBool value)
            _ -> Left (evalError context "&& expects Bool arguments")
        _ -> Left (evalError context "&& expects Bool arguments")
    shortCircuitOr = do
      left <- force context (arguments !! 0)
      case left of
        VBool True -> pure (VBool True)
        VBool False -> do
          right <- force context (arguments !! 1)
          case right of
            VBool value -> pure (VBool value)
            _ -> Left (evalError context "|| expects Bool arguments")
        _ -> Left (evalError context "|| expects Bool arguments")
    pairFirst = do
      value <- force context (arguments !! 0)
      case value of
        VCon constructor [first, _] | constructor == builtinName "(,)" -> force context first
        _ -> Left (evalError context "fst expects a pair")
    pairSecond = do
      value <- force context (arguments !! 0)
      case value of
        VCon constructor [_, second] | constructor == builtinName "(,)" -> force context second
        _ -> Left (evalError context "snd expects a pair")
    putPrimitive newline = do
      text <- expectStringValue context (arguments !! 0)
      pure
        ( VIO
            ( do
                putStr text
                if newline then putStrLn "" else pure ()
                pure (Right VUnit)
            )
        )
    intArgument index = do
      value <- force context (arguments !! index)
      case value of
        VInt number -> pure number
        _ -> Left (evalError context (name <> " expects an Int"))

fixValue :: EvalContext -> Value -> Value
fixValue context function = value
  where
    value =
      case apply context function (ThunkDelay (Right value)) of
        Right result -> result
        Left _ -> VUnit

-- List helpers --------------------------------------------------------------

consName :: ResolvedName
consName = builtinName ":"

nilName :: ResolvedName
nilName = builtinName "[]"

forceList :: EvalContext -> Thunk -> Either EvalError (Maybe (Thunk, Thunk))
forceList context thunk = case force context thunk of
  Right (VCon constructor [headThunk, tailThunk]) | constructor == consName -> Right (Just (headThunk, tailThunk))
  Right (VCon constructor []) | constructor == nilName -> Right Nothing
  Right _ -> Left (evalError context "expected a list")
  Left err -> Left err

mapList :: EvalContext -> Value -> Thunk -> Value
mapList context function list = case forceList context list of
  Left _ -> VCon nilName []
  Right Nothing -> VCon nilName []
  Right (Just (headThunk, tailThunk)) ->
    VCon
      consName
      [ ThunkDelay (apply context function headThunk)
      , ThunkDelay (Right (mapList context function tailThunk))
      ]

filterList :: EvalContext -> Value -> Thunk -> Value
filterList context function list = case forceList context list of
  Left _ -> VCon nilName []
  Right Nothing -> VCon nilName []
  Right (Just (headThunk, tailThunk)) ->
    case apply context function headThunk of
      Right (VBool True) ->
        VCon
          consName
          [ headThunk
          , ThunkDelay (Right (filterList context function tailThunk))
          ]
      Right (VBool False) -> filterList context function tailThunk
      _ -> VCon nilName []

foldrList :: EvalContext -> Value -> Thunk -> Thunk -> Value
foldrList context function initial list = case forceList context list of
  Left _ -> VCon nilName []
  Right Nothing -> forceValue context initial
  Right (Just (headThunk, tailThunk)) ->
    case apply context function headThunk of
      Left _ -> VCon nilName []
      Right partiallyApplied ->
        case apply context partiallyApplied (ThunkDelay (Right (foldrList context function initial tailThunk))) of
          Right value -> value
          Left _ -> VCon nilName []

foldlList :: EvalContext -> Value -> Value -> Thunk -> Value
foldlList context function initial list = case forceList context list of
  Left _ -> initial
  Right Nothing -> initial
  Right (Just (headThunk, tailThunk)) ->
    case apply context function (ThunkValue initial) of
      Left _ -> initial
      Right partiallyApplied ->
        case apply context partiallyApplied headThunk of
          Right next -> foldlList context function next tailThunk
          Left _ -> initial

lengthList :: EvalContext -> Thunk -> Either EvalError Int
lengthList context list = go 0 list
  where
    go count thunk = case forceList context thunk of
      Left err -> Left err
      Right Nothing -> Right count
      Right (Just (_, tailThunk)) -> go (count + 1) tailThunk

appendLists :: EvalContext -> Thunk -> Thunk -> Value
appendLists context left right = case forceList context left of
  Left _ -> VCon nilName []
  Right Nothing -> forceValue context right
  Right (Just (headThunk, tailThunk)) ->
    VCon consName [headThunk, ThunkDelay (Right (appendLists context tailThunk right))]

takeList :: EvalContext -> Int -> Thunk -> Value
takeList context count list
  | count <= 0 = VCon nilName []
  | otherwise = case forceList context list of
      Left _ -> VCon nilName []
      Right Nothing -> VCon nilName []
      Right (Just (headThunk, tailThunk)) ->
        VCon consName [headThunk, ThunkDelay (Right (takeList context (count - 1) tailThunk))]

dropList :: EvalContext -> Int -> Thunk -> Value
dropList context count list
  | count <= 0 = forceValue context list
  | otherwise = case forceList context list of
      Left _ -> VCon nilName []
      Right Nothing -> VCon nilName []
      Right (Just (_, tailThunk)) -> dropList context (count - 1) tailThunk

reverseList :: EvalContext -> Thunk -> Value
reverseList context list = go [] list
  where
    go acc thunk = case forceList context thunk of
      Left _ -> build acc
      Right Nothing -> build acc
      Right (Just (headThunk, tailThunk)) -> go (headThunk : acc) tailThunk
    build [] = VCon nilName []
    build (thunk : rest) = VCon consName [thunk, ThunkDelay (Right (build rest))]

concatLists :: EvalContext -> Thunk -> Value
concatLists context list = case forceList context list of
  Left _ -> VCon nilName []
  Right Nothing -> VCon nilName []
  Right (Just (headThunk, tailThunk)) ->
    appendLists context headThunk (ThunkDelay (Right (concatLists context tailThunk)))

concatMapList :: EvalContext -> Value -> Thunk -> Value
concatMapList context function list = case forceList context list of
  Left _ -> VCon nilName []
  Right Nothing -> VCon nilName []
  Right (Just (headThunk, tailThunk)) ->
    case apply context function headThunk of
      Right value -> appendLists context (ThunkValue value) (ThunkDelay (Right (concatMapList context function tailThunk)))
      Left _ -> VCon nilName []

zipLists :: EvalContext -> Thunk -> Thunk -> Value
zipLists context left right = case (forceList context left, forceList context right) of
  (Right (Just (leftHead, leftTail)), Right (Just (rightHead, rightTail))) ->
    VCon
      consName
      [ ThunkDelay (Right (VCon (builtinName "(,)") [leftHead, rightHead]))
      , ThunkDelay (Right (zipLists context leftTail rightTail))
      ]
  _ -> VCon nilName []

enumFromList :: Integer -> Value
enumFromList start = VCon consName [ThunkValue (VInt start), ThunkDelay (Right (enumFromList (start + 1)))]

enumFromThenList :: Integer -> Integer -> Value
enumFromThenList start step = VCon consName [ThunkValue (VInt start), ThunkDelay (Right (enumFromThenList (start + step) step))]

enumFromToList :: Integer -> Integer -> Value
enumFromToList start end
  | start > end = VCon nilName []
  | otherwise = VCon consName [ThunkValue (VInt start), ThunkDelay (Right (enumFromToList (start + 1) end))]

enumFromThenToList :: Integer -> Integer -> Integer -> Value
enumFromThenToList start step end
  | step == 0 = if start <= end then VCon consName [ThunkValue (VInt start), ThunkDelay (Right (enumFromThenToList start step end))] else VCon nilName []
  | step > 0 && start > end = VCon nilName []
  | step < 0 && start < end = VCon nilName []
  | otherwise = VCon consName [ThunkValue (VInt start), ThunkDelay (Right (enumFromThenToList (start + step) step end))]

stringValue :: String -> Value
stringValue values = foldr (\character rest -> VCon consName [ThunkValue (VChar character), ThunkDelay (Right rest)]) (VCon nilName []) values

-- IO helpers ----------------------------------------------------------------

runIOValue :: Value -> IO (Either EvalError Value)
runIOValue value = case value of
  VIO action -> action
  _ -> pure (Right value)

bindIO :: EvalContext -> Value -> Value -> Value
bindIO context action function = VIO $ do
  result <- runIOValue action
  case result of
    Left err -> pure (Left err)
    Right value -> case apply context function (ThunkValue value) of
      Left err -> pure (Left err)
      Right next -> runIOValue next

thenIO :: EvalContext -> Value -> Value -> Value
thenIO context first second = VIO $ do
  result <- runIOValue first
  case result of
    Left err -> pure (Left err)
    Right _ -> runIOValue second

-- Display -------------------------------------------------------------------

showValue :: Value -> String
showValue value = case value of
  VInt number -> show number
  VBool boolean -> if boolean then "True" else "False"
  VChar character -> show character
  VUnit -> "()"
  VCon constructor [] | constructor == nilName -> "[]"
  VCon constructor _ | constructor == consName -> "<list>"
  VCon constructor fields -> "(" <> resolvedShow constructor <> " " <> unwords (map (const "<thunk>") fields) <> ")"
  VClosure {} -> "<function>"
  VPrimitive name _ -> "<primitive " <> name <> ">"
  VConFun constructor _ -> "<constructor " <> resolvedShow constructor <> ">"
  VDict className _ -> "<dictionary " <> className <> ">"
  VIO _ -> "<io>"

showValueIn :: EvalContext -> Value -> String
showValueIn context value = case value of
  VCon constructor fields
    | constructor == nilName -> "[]"
    | constructor == consName ->
        case listString fields of
          Just text -> show text
          Nothing -> "[" <> goList 0 fields <> "]"
    | constructor == builtinName "(,)" && length fields == 2 ->
        "(" <> showThunk (head fields) <> ", " <> showThunk (fields !! 1) <> ")"
    | null fields -> resolvedOriginal constructor
    | otherwise -> "(" <> resolvedOriginal constructor <> " " <> unwords (map showThunk fields) <> ")"
  _ -> showValue value
  where
    goList count _ | count >= 50 = "..."
    goList count [headThunk, tailThunk] =
      case force context tailThunk of
        Right (VCon constructor []) | constructor == nilName -> showThunk headThunk
        Right (VCon constructor [nextHead, nextTail]) | constructor == consName ->
          showThunk headThunk <> ", " <> goList (count + 1) [nextHead, nextTail]
        _ -> showThunk headThunk <> ", ..."
    goList _ _ = ""
    showThunk thunk = case force context thunk of
      Right value' -> showValueIn context value'
      Left err -> "<error: " <> evalErrorMessage err <> ">"
    listString [headThunk, tailThunk] =
      case force context headThunk of
        Right (VChar character) -> (character :) <$> listStringRest 1 tailThunk
        _ -> Nothing
    listString _ = Nothing
    listStringRest count _ | count >= 200 = Nothing
    listStringRest count thunk = case force context thunk of
      Right (VCon constructor []) | constructor == nilName -> Just ""
      Right (VCon constructor [nextHead, nextTail]) | constructor == consName ->
        case force context nextHead of
          Right (VChar character) -> (character :) <$> listStringRest (count + 1) nextTail
          _ -> Nothing
      _ -> Nothing

renderValue :: CoreModule -> Value -> String
renderValue core value =
  showValueIn (EvalContext (coreConstructors core) "<value>") value

valueToString :: EvalContext -> Value -> Either EvalError String
valueToString context value = case value of
  VCon constructor [headThunk, tailThunk] | constructor == consName -> do
    headValue <- force context headThunk
    case headValue of
      VChar character -> do
        rest <- valueToString context =<< force context tailThunk
        pure (character : rest)
      _ -> Left (evalError context "expected a String")
  VCon constructor [] | constructor == nilName -> Right ""
  _ -> Left (evalError context "expected a String")

expectStringValue :: EvalContext -> Thunk -> Either EvalError String
expectStringValue context thunk = do
  value <- force context thunk
  valueToString context value

expectStringEither :: EvalContext -> Thunk -> EvalError
expectStringEither context thunk =
  case expectStringValue context thunk of
    Left err -> err
    Right _ -> evalError context "expected a String"

evalError :: EvalContext -> String -> EvalError
evalError context message = EvalError (contextSource context) message

forceValue :: EvalContext -> Thunk -> Value
forceValue context thunk = case force context thunk of
  Right value -> value
  Left _ -> VUnit
