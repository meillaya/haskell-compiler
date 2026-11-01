-- | Recursive-descent parser for ProtoHaskell.
--
-- The lexer has already inserted layout braces and semicolons, so the parser
-- sees an explicit block structure.  Fixity declarations are collected in a
-- prepass because expressions can mention operators before the declaration
-- that gives them a fixity.
module WYAH.Parser
  ( ParseError(..)
  , parseModule
  , parseExpr
  , parseTypeText
  , parseReplInput
  , parseDeclarations
  , collectFixities
  ) where

import Data.Char (isUpper)
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import WYAH.Diagnostic
import WYAH.Lexer
import WYAH.Span
import WYAH.Syntax

data ParseError = ParseError
  { parseErrorFile :: FilePath
  , parseErrorSpan :: Span
  , parseErrorMessage :: String
  }
  deriving (Eq)

instance Show ParseError where
  show err =
    renderDiagnostic
      Diagnostic
        { diagnosticPhase = phaseName ParsePhase
        , diagnosticSource = Just (parseErrorFile err)
        , diagnosticSpan = Just (parseErrorSpan err)
        , diagnosticMessage = parseErrorMessage err
        }

data ParserState = ParserState
  { psTokens :: [Token]
  , psFile :: FilePath
  , psFixities :: FixityTable
  , psLastSpan :: Span
  }

newtype Parser a = Parser {runParser :: ParserState -> Either ParseError (a, ParserState)}

instance Functor Parser where
  fmap f (Parser action) = Parser $ \state ->
    case action state of
      Left err -> Left err
      Right (value, state') -> Right (f value, state')

instance Applicative Parser where
  pure value = Parser $ \state -> Right (value, state)
  Parser pf <*> Parser pa = Parser $ \state ->
    case pf state of
      Left err -> Left err
      Right (f, state') ->
        case pa state' of
          Left err -> Left err
          Right (value, state'') -> Right (f value, state'')

instance Monad Parser where
  Parser action >>= f = Parser $ \state ->
    case action state of
      Left err -> Left err
      Right (value, state') -> runParser (f value) state'

-- Public entry points -------------------------------------------------------

parseModule :: FilePath -> String -> Either ParseError Module
parseModule file input = do
  tokens <- lexOrFail LayoutModule file input
  let fixities = builtinFixities <> collectFixities tokens
  fst <$> runParser parseModuleTokens (ParserState tokens file fixities NoSpan)

parseExpr :: FilePath -> String -> Either ParseError Expr
parseExpr file input = do
  tokens <- lexOrFail LayoutExpression file input
  let fixities = builtinFixities <> collectFixities tokens
  fst <$> runParser parseExprTokens (ParserState tokens file fixities NoSpan)

parseTypeText :: FilePath -> String -> Either ParseError Type
parseTypeText file input = do
  tokens <- lexOrFail LayoutExpression file input
  let fixities = builtinFixities <> collectFixities tokens
  fst <$> runParser parseTypeTokens (ParserState tokens file fixities NoSpan)

parseTypeTokens :: Parser Type
parseTypeTokens = do
  type_ <- parseType
  _ <- expectKind (== TkEOF) "end of input"
  pure type_

parseDeclarations :: FilePath -> String -> Either ParseError [Decl]
parseDeclarations file input = moduleDecls <$> parseModule file input

-- | Try a declaration module first, then a bare expression.  The REPL needs
-- both shapes on the same input line.
parseReplInput :: FilePath -> String -> Either ParseError (Either Expr Module)
parseReplInput file input =
  case parseModule file input of
    Right parsedModule -> Right (Right parsedModule)
    Left moduleError ->
      case parseExpr file input of
        Right expr -> Right (Left expr)
        Left exprError ->
          case firstSignificantToken input of
            Just kind
              | kindIsDeclaration kind -> Left moduleError
            _ -> Left exprError

lexOrFail :: LayoutMode -> FilePath -> String -> Either ParseError [Token]
lexOrFail mode file input =
  case lexAndLayout mode file input of
    Left err ->
      Left
        ParseError
          { parseErrorFile = lexErrorSource err
          , parseErrorSpan = lexErrorSpan err
          , parseErrorMessage = lexErrorMessage err
          }
    Right tokens -> Right tokens

firstSignificantToken :: String -> Maybe TokenKind
firstSignificantToken input =
  case lexTokens "<repl>" input of
    Right (token : _) -> Just (tokenKind token)
    _ -> Nothing

kindIsDeclaration :: TokenKind -> Bool
kindIsDeclaration kind = kind `elem`
  [ TkKeyword "data"
  , TkKeyword "type"
  , TkKeyword "class"
  , TkKeyword "instance"
  , TkKeyword "infixl"
  , TkKeyword "infixr"
  , TkKeyword "infix"
  , TkKeyword "module"
  ]

-- Basic parser plumbing -----------------------------------------------------

parseModuleTokens :: Parser Module
parseModuleTokens = do
  start <- currentStart
  name <- parseOptionalModuleHeader
  _ <- expectKind (== TkLBrace) "the beginning of the declaration block"
  decls <- parseDeclBlock
  _ <- expectKind (== TkRBrace) "the end of the declaration block"
  _ <- expectKind (== TkEOF) "end of input"
  end <- lastEnd
  pure (Module (mkSpan start end) name decls)

parseExprTokens :: Parser Expr
parseExprTokens = do
  expr <- parseExpression
  _ <- expectKind (== TkEOF) "end of input"
  pure expr

parseOptionalModuleHeader :: Parser (Maybe Name)
parseOptionalModuleHeader = do
  isModule <- matchKind (== TkKeyword "module")
  case isModule of
    Nothing -> pure Nothing
    Just _ -> do
      name <- expectConName
      _ <- expectKeyword "where"
      pure (Just name)

peekToken :: Parser Token
peekToken = Parser $ \state ->
  case psTokens state of
    token : _ -> Right (token, state)
    [] ->
      let position = endPositionOf (psLastSpan state)
       in Right (Token (Span position position) TkEOF, state)

peekKind :: Parser TokenKind
peekKind = tokenKind <$> peekToken

peekAt :: Int -> Parser Token
peekAt index = Parser $ \state ->
  let tokens = psTokens state
      token = case drop index tokens of
        next : _ -> next
        [] ->
          let position = endPositionOf (psLastSpan state)
           in Token (Span position position) TkEOF
   in Right (token, state)

advance :: Parser Token
advance = Parser $ \state ->
  case psTokens state of
    token : rest ->
      Right (token, state {psTokens = rest, psLastSpan = tokenSpan token})
    [] ->
      let position = endPositionOf (psLastSpan state)
          token = Token (Span position position) TkEOF
       in Right (token, state {psLastSpan = tokenSpan token})

currentStart :: Parser Position
currentStart = do
  state <- Parser $ \s -> Right (s, s)
  case psTokens state of
    token : _ -> pure (spanStart (tokenSpan token))
    [] -> pure (endPositionOf (psLastSpan state))

lastEnd :: Parser Position
lastEnd = do
  state <- Parser $ \s -> Right (s, s)
  pure (endPositionOf (psLastSpan state))

endPositionOf :: Span -> Position
endPositionOf span_ = fromMaybe startOfInput (spanEndPosition span_)

mkSpan :: Position -> Position -> Span
mkSpan start end
  | end < start = Span start start
  | otherwise = Span start end

failParse :: String -> Parser a
failParse message = Parser $ \state ->
  Left
    ParseError
      { parseErrorFile = psFile state
      , parseErrorSpan = errorSpanOf state
      , parseErrorMessage = message
      }

failParseAt :: Span -> String -> Parser a
failParseAt span_ message = Parser $ \state ->
  Left
    ParseError
      { parseErrorFile = psFile state
      , parseErrorSpan = span_
      , parseErrorMessage = message
      }

errorSpanOf :: ParserState -> Span
errorSpanOf state =
  case psTokens state of
    token : _ ->
      case tokenKind token of
        TkEOF -> Span position position
        _ -> tokenSpan token
    [] ->
      let position = endPositionOf (psLastSpan state)
       in Span position position
  where
    position = endPositionOf (psLastSpan state)

expectKind :: (TokenKind -> Bool) -> String -> Parser Token
expectKind predicate expected = Parser $ \state ->
  case psTokens state of
    token : rest
      | predicate (tokenKind token) ->
          Right (token, state {psTokens = rest, psLastSpan = tokenSpan token})
      | otherwise ->
          Left
            ParseError
              { parseErrorFile = psFile state
              , parseErrorSpan = tokenSpan token
              , parseErrorMessage = "expected " <> expected <> ", found " <> describeToken token
              }
    [] ->
      let position = endPositionOf (psLastSpan state)
       in Left
            ParseError
              { parseErrorFile = psFile state
              , parseErrorSpan = Span position position
              , parseErrorMessage = "expected " <> expected <> ", found end of input"
              }

matchKind :: (TokenKind -> Bool) -> Parser (Maybe Token)
matchKind predicate = Parser $ \state ->
  case psTokens state of
    token : rest
      | predicate (tokenKind token) ->
          Right (Just token, state {psTokens = rest, psLastSpan = tokenSpan token})
    _ -> Right (Nothing, state)

expectKeyword :: String -> Parser Token
expectKeyword keyword = expectKind (== TkKeyword keyword) ("keyword " <> keyword)

expectOp :: Parser (Name, Bool)
expectOp = do
  token <- expectKind isOperatorKind "an operator"
  pure (operatorNameFromToken token)

isOperatorKind :: TokenKind -> Bool
isOperatorKind (TkOp _) = True
isOperatorKind _ = False

operatorNameFromToken :: Token -> (Name, Bool)
operatorNameFromToken token =
  case tokenKind token of
    TkOp raw ->
      let name = stripBackticks raw
       in (name, isConstructorName name)
    _ -> ("", False)

expectVarName :: Parser Name
expectVarName = do
  token <- expectKind isVarKind "a variable name"
  case tokenKind token of
    TkVar name -> pure name
    _ -> failParse "expected a variable name"

isVarKind :: TokenKind -> Bool
isVarKind (TkVar _) = True
isVarKind _ = False

expectConName :: Parser Name
expectConName = do
  token <- expectKind isConKind "a constructor name"
  case tokenKind token of
    TkCon name -> pure name
    _ -> failParse "expected a constructor name"

isConKind :: TokenKind -> Bool
isConKind (TkCon _) = True
isConKind _ = False

skipSemis :: Parser ()
skipSemis = do
  next <- matchKind (== TkSemi)
  case next of
    Just _ -> skipSemis
    Nothing -> pure ()

many :: Parser a -> Parser [a]
many parser = go []
  where
    go acc = do
      result <- tryP parser
      case result of
        Nothing -> pure (reverse acc)
        Just value -> go (value : acc)

many1 :: Parser a -> Parser [a]
many1 parser = do
  first <- parser
  rest <- many parser
  pure (first : rest)

manyWhile :: (TokenKind -> Bool) -> Parser a -> Parser [a]
manyWhile predicate parser = go []
  where
    go acc = do
      kind <- peekKind
      if predicate kind
        then do
          value <- parser
          go (value : acc)
        else pure (reverse acc)

tryP :: Parser a -> Parser (Maybe a)
tryP (Parser action) = Parser $ \state ->
  case action state of
    Left _ -> Right (Nothing, state)
    Right (value, state') -> Right (Just value, state')

withSpan :: Parser a -> Parser (Span, a)
withSpan parser = do
  start <- currentStart
  value <- parser
  end <- lastEnd
  pure (mkSpan start end, value)

stripBackticks :: Name -> Name
stripBackticks name
  | length name >= 2
  , head name == '`'
  , last name == '`' =
      init (tail name)
  | otherwise = name

-- Fixity prepass ------------------------------------------------------------

collectFixities :: [Token] -> FixityTable
collectFixities = scan
  where
    scan [] = []
    scan (token : rest) =
      case tokenKind token of
        TkKeyword "infixl" -> collect (Infix AssocLeft) rest
        TkKeyword "infixr" -> collect (Infix AssocRight) rest
        TkKeyword "infix" -> collect (Infix AssocNone) rest
        _ -> scan rest

    collect makeFixity tokens =
      let (precedence, afterPrecedence) = case tokens of
            next : more | TkInt value <- tokenKind next -> (fromIntegral value, more)
            _ -> (9, tokens)
          (ops, remaining) = collectOperatorList afterPrecedence
       in [(name, makeFixity precedence) | name <- ops] <> scan remaining

    collectOperatorList (token : rest) =
      case tokenKind token of
        TkOp raw -> collectMore (stripBackticks raw) rest
        _ -> ([], token : rest)
    collectOperatorList [] = ([], [])

    collectMore name (token : rest) =
      case tokenKind token of
        TkComma ->
          case collectOperatorList rest of
            (more, remaining) -> (name : more, remaining)
        _ -> ([name], token : rest)
    collectMore name [] = ([name], [])

-- Declarations --------------------------------------------------------------

parseDeclBlock :: Parser [Decl]
parseDeclBlock = do
  skipSemis
  kind <- peekKind
  if kind == TkRBrace || kind == TkEOF
    then pure []
    else do
      decl <- parseDecl
      next <- peekKind
      case next of
        TkSemi -> do
          skipSemis
          rest <- parseDeclBlock
          pure (decl : rest)
        TkRBrace -> pure [decl]
        TkEOF -> pure [decl]
        _ -> failParse ("expected ';' after declaration, found " <> describeTokenKind next)

describeTokenKind :: TokenKind -> String
describeTokenKind kind = case kind of
  TkVar name -> "identifier " <> name
  TkCon name -> "constructor " <> name
  TkOp op -> "operator " <> op
  TkKeyword keyword -> "keyword " <> keyword
  TkInt n -> "integer " <> show n
  TkChar c -> "character " <> show c
  TkString s -> "string " <> show s
  TkLParen -> "("
  TkRParen -> ")"
  TkLBracket -> "["
  TkRBracket -> "]"
  TkLBrace -> "{"
  TkRBrace -> "}"
  TkComma -> ","
  TkSemi -> ";"
  TkDoubleColon -> "::"
  TkEquals -> "="
  TkPipe -> "|"
  TkArrow -> "->"
  TkLArrow -> "<-"
  TkDArrow -> "=>"
  TkLambda -> "\\"
  TkAt -> "@"
  TkUnderscore -> "_"
  TkDotDot -> ".."
  TkEOF -> "end of input"

parseDecl :: Parser Decl
parseDecl = do
  kind <- peekKind
  case kind of
    TkKeyword "data" -> parseDataDecl
    TkKeyword "type" -> parseTypeSynonym
    TkKeyword "class" -> parseClassDecl
    TkKeyword "instance" -> parseInstanceDecl
    TkKeyword "infixl" -> parseFixityDecl AssocLeft
    TkKeyword "infixr" -> parseFixityDecl AssocRight
    TkKeyword "infix" -> parseFixityDecl AssocNone
    TkVar _ -> do
      signature <- tryP parseSignature
      case signature of
        Just decl -> pure decl
        Nothing -> parseFunctionDecl
    TkCon _ -> failParse "top-level constructor declarations are not supported; use 'data'"
    TkLParen -> do
      signature <- tryP parseSignature
      case signature of
        Just decl -> pure decl
        Nothing -> parseFunctionDecl
    _ -> failParse ("unexpected token at the beginning of a declaration: " <> describeTokenKind kind)

parseSignature :: Parser Decl
parseSignature = do
  start <- currentStart
  name <- parseDeclName
  _ <- expectKind (== TkDoubleColon) "'::'"
  type_ <- parseType
  end <- lastEnd
  pure (DSig (mkSpan start end) name type_)

parseDeclName :: Parser Name
parseDeclName = do
  kind <- peekKind
  case kind of
    TkVar name -> advance >> pure name
    TkLParen -> do
      _ <- advance
      (name, _) <- expectOp
      _ <- expectKind (== TkRParen) "')'"
      pure name
    _ -> failParse "expected a declaration name"

parseFunctionDecl :: Parser Decl
parseFunctionDecl = do
  start <- currentStart
  (name, patterns) <- parseFunctionLhs
  rhs <- parseFunctionRhs
  whereDecls <- parseWhereBlock
  end <- lastEnd
  pure (DFun (mkSpan start end) name patterns rhs whereDecls)

parseFunctionLhs :: Parser (Name, [Pattern])
parseFunctionLhs = do
  kind <- peekKind
  case kind of
    TkVar name -> do
      nameToken <- advance
      next <- peekKind
      case next of
        TkEquals -> pure (name, [])
        TkPipe -> pure (name, [])
        _
          | startsAPat next -> do
              patterns <- many1 parseAPat
              pure (name, patterns)
          | isOperatorKind next -> do
              (operator, isConstructor) <- expectOp
              if isConstructor
                then failParse "constructor operators cannot be defined as functions"
                else do
                  second <- parseAPat
                  pure (operator, [PVar (tokenSpan nameToken) name, second])
          | otherwise ->
              failParse ("expected function arguments, '=' or '|', found " <> describeTokenKind next)
    TkLParen -> do
      parsed <- tryP parsePrefixOperatorLhs
      case parsed of
        Just result -> pure result
        Nothing -> failParse "top-level pattern bindings are not supported; define a named function instead"
    _ -> failParse "expected a function name"

parsePrefixOperatorLhs :: Parser (Name, [Pattern])
parsePrefixOperatorLhs = do
  _ <- expectKind (== TkLParen) "'('"
  (operator, isConstructor) <- expectOp
  if isConstructor
    then failParse "constructor operators cannot be defined as functions"
    else do
      _ <- expectKind (== TkRParen) "')'"
      patterns <- many parseAPat
      pure (operator, patterns)

parseFunctionRhs :: Parser Rhs
parseFunctionRhs = do
  kind <- peekKind
  case kind of
    TkEquals -> do
      start <- currentStart
      _ <- advance
      expr <- parseExpression
      end <- lastEnd
      pure (PlainRhs (mkSpan start end) expr)
    TkPipe -> do
      start <- currentStart
      guards <- many1 parseGuardClause
      end <- lastEnd
      pure (GuardedRhs (mkSpan start end) guards)
    _ -> failParse ("expected '=' or '|' after a function left-hand side, found " <> describeTokenKind kind)

parseGuardClause :: Parser (Guard, Expr)
parseGuardClause = do
  _ <- expectKind (== TkPipe) "'|'"
  guard_ <- parseGuardSequence
  _ <- expectKind (== TkEquals) "'=' after a guard"
  expr <- parseExpression
  pure (guard_, expr)

parseGuardSequence :: Parser Guard
parseGuardSequence = do
  first <- parseGuard
  rest <- many $ do
    _ <- expectKind (== TkComma) "',' between guard conditions"
    parseGuard
  case rest of
    [] -> pure first
    _ -> pure (GuardList (spanBetween (guardSpan first) (guardSpan (last rest))) (first : rest))

parseGuard :: Parser Guard
parseGuard = do
  start <- currentStart
  patternGuard <- tryP $ do
    pattern_ <- parsePattern
    _ <- expectKind (== TkLArrow) "'<-' in a pattern guard"
    expr <- parseExpression
    pure (pattern_, expr)
  case patternGuard of
    Just (pattern_, expr) -> do
      end <- lastEnd
      pure (GuardPattern (mkSpan start end) pattern_ expr)
    Nothing -> do
      expr <- parseExpression
      end <- lastEnd
      pure (GuardExpr (mkSpan start end) expr)

parseWhereBlock :: Parser [Decl]
parseWhereBlock = do
  whereToken <- matchKind (== TkKeyword "where")
  case whereToken of
    Nothing -> pure []
    Just _ -> do
      _ <- expectKind (== TkLBrace) "the beginning of a where block"
      decls <- parseDeclBlock
      _ <- expectKind (== TkRBrace) "the end of a where block"
      pure decls

-- Data declarations ---------------------------------------------------------

parseDataDecl :: Parser Decl
parseDataDecl = do
  start <- currentStart
  _ <- expectKeyword "data"
  name <- expectConName
  variables <- manyWhile isVarKind expectVarName
  kind <- peekKind
  constructors <-
    case kind of
      TkKeyword "where" -> do
        _ <- advance
        _ <- expectKind (== TkLBrace) "the beginning of a data constructor block"
        constructors <- parseGadtConstructors
        _ <- expectKind (== TkRBrace) "the end of a data constructor block"
        pure constructors
      TkEquals -> do
        _ <- advance
        parseRegularConstructors
      _ -> failParse "expected '=' or 'where' in a data declaration"
  derivingClasses <- parseDeriving
  end <- lastEnd
  pure
    ( DData
        (mkSpan start end)
        DataDecl
          { dataDeclSpan = mkSpan start end
          , dataDeclName = name
          , dataDeclVars = variables
          , dataDeclCons = constructors
          , dataDeclDeriving = derivingClasses
          }
    )

parseRegularConstructors :: Parser [ConDecl]
parseRegularConstructors = do
  first <- parseRegularConstructor
  rest <- many $ do
    _ <- expectKind (== TkPipe) "'|' between constructors"
    parseRegularConstructor
  pure (first : rest)

parseRegularConstructor :: Parser ConDecl
parseRegularConstructor = do
  start <- currentStart
  name <- expectConName
  kind <- peekKind
  (fields, recordFields) <-
    if kind == TkLBrace
      then do
        record <- parseRecordFields
        pure (map snd record, record)
      else do
        fields <- manyWhile startsTypeAtom parseAType
        pure (fields, [])
  end <- lastEnd
  pure
    ConDecl
      { conDeclSpan = mkSpan start end
      , conDeclName = name
      , conDeclFields = fields
      , conDeclResult = Nothing
      , conDeclRecord = recordFields
      }

parseGadtConstructors :: Parser [ConDecl]
parseGadtConstructors = do
  skipSemis
  kind <- peekKind
  if kind == TkRBrace || kind == TkEOF
    then pure []
    else do
      constructor <- parseGadtConstructor
      skipSemis
      rest <- parseGadtConstructors
      pure (constructor : rest)

parseGadtConstructor :: Parser ConDecl
parseGadtConstructor = do
  start <- currentStart
  name <- expectConName
  _ <- expectKind (== TkDoubleColon) "'::' in a GADT constructor"
  kind <- peekKind
  (fields, result, recordFields) <-
    if kind == TkLBrace
      then do
        record <- parseRecordFields
        _ <- expectKind (== TkArrow) "'->' after record fields"
        resultType <- parseType
        pure (map snd record, Just resultType, record)
      else do
        fullType <- parseType
        let (arguments, resultType) = splitFunctionType fullType
        pure (arguments, Just resultType, [])
  end <- lastEnd
  pure
    ConDecl
      { conDeclSpan = mkSpan start end
      , conDeclName = name
      , conDeclFields = fields
      , conDeclResult = result
      , conDeclRecord = recordFields
      }

parseRecordFields :: Parser [(Name, Type)]
parseRecordFields = do
  _ <- expectKind (== TkLBrace) "'{' before record fields"
  skipCommas
  kind <- peekKind
  if kind == TkRBrace
    then do
      _ <- advance
      pure []
    else do
      first <- parseRecordField
      rest <- many $ do
        _ <- expectKind (== TkComma) "',' between record fields"
        skipCommas
        parseRecordField
      _ <- expectKind (== TkRBrace) "'}' after record fields"
      pure (first : rest)

parseRecordField :: Parser (Name, Type)
parseRecordField = do
  name <- expectVarName
  _ <- expectKind (== TkDoubleColon) "'::' after a record field name"
  type_ <- parseType
  pure (name, type_)

splitFunctionType :: Type -> ([Type], Type)
splitFunctionType (TyFun _ argument result) =
  let (arguments, finalResult) = splitFunctionType result
   in (argument : arguments, finalResult)
splitFunctionType type_ = ([], type_)

parseDeriving :: Parser [Name]
parseDeriving = do
  derivingToken <- matchKind (== TkKeyword "deriving")
  case derivingToken of
    Nothing -> pure []
    Just _ -> do
      kind <- peekKind
      case kind of
        TkLParen -> do
          _ <- advance
          classes <- parseCommaSeparated expectConName
          _ <- expectKind (== TkRParen) "')' after deriving classes"
          pure classes
        TkCon name -> do
          _ <- advance
          pure [name]
        _ -> failParse "expected a class name or '(classes)' after deriving"

parseCommaSeparated :: Parser a -> Parser [a]
parseCommaSeparated parser = do
  first <- parser
  rest <- many $ do
    _ <- expectKind (== TkComma) "','"
    parser
  pure (first : rest)

skipCommas :: Parser ()
skipCommas = do
  next <- matchKind (== TkComma)
  case next of
    Just _ -> skipCommas
    Nothing -> pure ()

-- Type synonyms, classes, instances, fixities ------------------------------

parseTypeSynonym :: Parser Decl
parseTypeSynonym = do
  start <- currentStart
  _ <- expectKeyword "type"
  name <- expectConName
  variables <- manyWhile isVarKind expectVarName
  _ <- expectKind (== TkEquals) "'=' in a type synonym"
  type_ <- parseType
  end <- lastEnd
  pure
    ( DType
        (mkSpan start end)
        TypeSyn
          { typeSynSpan = mkSpan start end
          , typeSynName = name
          , typeSynVars = variables
          , typeSynRhs = type_
          }
    )

parseClassDecl :: Parser Decl
parseClassDecl = do
  start <- currentStart
  _ <- expectKeyword "class"
  context <- parseOptionalContext
  name <- expectConName
  variable <- expectVarName
  body <- parseOptionalWhereDecls
  end <- lastEnd
  pure
    ( DClass
        (mkSpan start end)
        ClassDecl
          { classDeclSpan = mkSpan start end
          , classDeclContext = context
          , classDeclName = name
          , classDeclVar = variable
          , classDeclDecls = body
          }
    )

parseInstanceDecl :: Parser Decl
parseInstanceDecl = do
  start <- currentStart
  _ <- expectKeyword "instance"
  context <- parseOptionalContext
  className <- expectConName
  headType <- parseBType
  body <- parseOptionalWhereDecls
  end <- lastEnd
  pure
    ( DInst
        (mkSpan start end)
        InstDecl
          { instDeclSpan = mkSpan start end
          , instDeclContext = context
          , instDeclClass = className
          , instDeclHead = headType
          , instDeclDecls = body
          }
    )

parseOptionalContext :: Parser [Pred]
parseOptionalContext = do
  result <- tryP $ do
    type_ <- parseTypeNoContext
    _ <- expectKind (== TkDArrow) "'=>'"
    case typeToPredicates type_ of
      Left message -> failParse message
      Right predicates -> pure predicates
  pure (fromMaybe [] result)

parseOptionalWhereDecls :: Parser [Decl]
parseOptionalWhereDecls = do
  whereToken <- matchKind (== TkKeyword "where")
  case whereToken of
    Nothing -> pure []
    Just _ -> do
      _ <- expectKind (== TkLBrace) "the beginning of a where block"
      decls <- parseDeclBlock
      _ <- expectKind (== TkRBrace) "the end of a where block"
      pure decls

parseFixityDecl :: Assoc -> Parser Decl
parseFixityDecl assoc = do
  start <- currentStart
  _ <- advance
  kind <- peekKind
  precedence <- case kind of
    TkInt value -> do
      _ <- advance
      pure (fromIntegral value)
    _ -> pure 9
  operators <- parseFixityOperators
  end <- lastEnd
  pure
    ( DFixity
        (mkSpan start end)
        FixitySpec
          { fixitySpecSpan = mkSpan start end
          , fixityAssoc = assoc
          , fixityPrecedence = precedence
          , fixityOps = operators
          }
    )

parseFixityOperators :: Parser [Name]
parseFixityOperators = do
  first <- parseFixityOperator
  rest <- many $ do
    _ <- expectKind (== TkComma) "',' between operators"
    parseFixityOperator
  pure (first : rest)

parseFixityOperator :: Parser Name
parseFixityOperator = do
  token <- expectKind isOperatorKind "an operator in a fixity declaration"
  pure (fst (operatorNameFromToken token))

-- Expressions ---------------------------------------------------------------

parseExpression :: Parser Expr
parseExpression = do
  kind <- peekKind
  case kind of
    TkKeyword "let" -> parseLetExpr
    TkKeyword "if" -> parseIfExpr
    TkKeyword "case" -> parseCaseExpr
    TkKeyword "do" -> parseDoExpr
    TkLambda -> parseLambdaExpr
    _ -> parseOperatorExpr

parseLetExpr :: Parser Expr
parseLetExpr = do
  start <- currentStart
  _ <- expectKeyword "let"
  _ <- expectKind (== TkLBrace) "the beginning of a let block"
  decls <- parseDeclBlock
  _ <- expectKind (== TkRBrace) "the end of a let block"
  _ <- expectKeyword "in"
  body <- parseExpression
  end <- lastEnd
  pure (ELet (mkSpan start end) decls body)

parseIfExpr :: Parser Expr
parseIfExpr = do
  start <- currentStart
  _ <- expectKeyword "if"
  condition <- parseExpression
  _ <- expectKeyword "then"
  trueBranch <- parseExpression
  _ <- expectKeyword "else"
  falseBranch <- parseExpression
  end <- lastEnd
  pure (EIf (mkSpan start end) condition trueBranch falseBranch)

parseCaseExpr :: Parser Expr
parseCaseExpr = do
  start <- currentStart
  _ <- expectKeyword "case"
  scrutinee <- parseExpression
  _ <- expectKeyword "of"
  _ <- expectKind (== TkLBrace) "the beginning of a case block"
  alternatives <- parseAlternatives
  _ <- expectKind (== TkRBrace) "the end of a case block"
  end <- lastEnd
  pure (ECase (mkSpan start end) scrutinee alternatives)

parseAlternatives :: Parser [Alt]
parseAlternatives = do
  skipSemis
  kind <- peekKind
  if kind == TkRBrace || kind == TkEOF
    then pure []
    else do
      alternative <- parseAlternative
      skipSemis
      rest <- parseAlternatives
      pure (alternative : rest)

parseAlternative :: Parser Alt
parseAlternative = do
  start <- currentStart
  pattern_ <- parsePattern
  rhs <- parseAlternativeRhs
  whereDecls <- parseWhereBlock
  end <- lastEnd
  pure
    Alt
      { altSpan = mkSpan start end
      , altPattern = pattern_
      , altRhs = rhs
      , altWhere = whereDecls
      }

parseAlternativeRhs :: Parser Rhs
parseAlternativeRhs = do
  kind <- peekKind
  case kind of
    TkArrow -> do
      start <- currentStart
      _ <- advance
      expr <- parseExpression
      end <- lastEnd
      pure (PlainRhs (mkSpan start end) expr)
    TkPipe -> do
      start <- currentStart
      guards <- many1 parseAlternativeGuard
      end <- lastEnd
      pure (GuardedRhs (mkSpan start end) guards)
    _ -> failParse ("expected '->' or '|' in a case alternative, found " <> describeTokenKind kind)

parseAlternativeGuard :: Parser (Guard, Expr)
parseAlternativeGuard = do
  _ <- expectKind (== TkPipe) "'|'"
  guard_ <- parseGuardSequence
  _ <- expectKind (== TkArrow) "'->' after a case guard"
  expr <- parseExpression
  pure (guard_, expr)

parseDoExpr :: Parser Expr
parseDoExpr = do
  start <- currentStart
  _ <- expectKeyword "do"
  _ <- expectKind (== TkLBrace) "the beginning of a do block"
  statements <- parseStatements
  _ <- expectKind (== TkRBrace) "the end of a do block"
  end <- lastEnd
  pure (EDo (mkSpan start end) statements)

parseStatements :: Parser [Stmt]
parseStatements = do
  skipSemis
  kind <- peekKind
  if kind == TkRBrace || kind == TkEOF
    then pure []
    else do
      statement <- parseStatement
      skipSemis
      rest <- parseStatements
      pure (statement : rest)

parseStatement :: Parser Stmt
parseStatement = do
  start <- currentStart
  kind <- peekKind
  case kind of
    TkKeyword "let" -> do
      _ <- advance
      _ <- expectKind (== TkLBrace) "the beginning of a let statement"
      decls <- parseDeclBlock
      _ <- expectKind (== TkRBrace) "the end of a let statement"
      end <- lastEnd
      pure (LetStmt (mkSpan start end) decls)
    _ -> do
      generator <- tryP $ do
        pattern_ <- parsePattern
        _ <- expectKind (== TkLArrow) "'<-' in a do generator"
        expr <- parseExpression
        pure (pattern_, expr)
      case generator of
        Just (pattern_, expr) -> do
          end <- lastEnd
          pure (Generator (mkSpan start end) pattern_ expr)
        Nothing -> do
          expr <- parseExpression
          end <- lastEnd
          pure (Qualifier (mkSpan start end) expr)

parseLambdaExpr :: Parser Expr
parseLambdaExpr = do
  start <- currentStart
  _ <- expectKind (== TkLambda) "'\\'"
  patterns <- many1 parseAPat
  _ <- expectKind (== TkArrow) "'->' after lambda patterns"
  body <- parseExpression
  end <- lastEnd
  pure (ELam (mkSpan start end) patterns body)

parseOperatorExpr :: Parser Expr
parseOperatorExpr = do
  left <- parseUnaryOrApplication
  parseOperatorRest 0 left

parseOperatorRest :: Int -> Expr -> Parser Expr
parseOperatorRest minimumPrecedence left = do
  operator <- peekOperator
  case operator of
    Nothing -> pure left
    Just (operatorSpan, operatorName, isConstructor) -> do
      fixity <- lookupFixityFor operatorName
      let precedence = fixityPrecedenceOf fixity
          associativity = fixityAssociativityOf fixity
      if precedence < minimumPrecedence
        then pure left
        else do
          nextKind <- peekAtKind 1
          if not (canStartExpr nextKind)
            then case nextKind of
              TkRParen -> pure left
              TkComma -> pure left
              TkRBracket -> pure left
              TkPipe -> pure left
              _ ->
                failParseAt
                  (Span (spanEnd operatorSpan) (spanEnd operatorSpan))
                  ("expected an expression after operator " <> operatorName)
            else do
              _ <- advance
              let nextMinimum =
                    case associativity of
                      AssocRight -> precedence
                      _ -> precedence + 1
              right <- parseUnaryOrApplication >>= parseOperatorRest nextMinimum
              whenNonAssociative associativity operatorName
              let combined = applyOperator operatorSpan operatorName isConstructor left right
              parseOperatorRest minimumPrecedence combined
  where
    peekOperator = do
      token <- peekToken
      case tokenKind token of
        TkOp raw ->
          let (name, isConstructor) = (stripBackticks raw, isConstructorName (stripBackticks raw))
           in pure (Just (tokenSpan token, name, isConstructor))
        _ -> pure Nothing

    lookupFixityFor name = Parser $ \state ->
      Right (lookupFixity (psFixities state) name, state)

    whenNonAssociative AssocNone name = do
      next <- peekToken
      case tokenKind next of
        TkOp raw
          | stripBackticks raw == name -> failParse ("non-associative operator " <> name <> " cannot be chained")
        _ -> pure ()
    whenNonAssociative _ _ = pure ()

fixityPrecedenceOf :: Fixity -> Int
fixityPrecedenceOf fixity = case fixity of
  Infix _ precedence -> precedence
  Prefix precedence -> precedence
  Postfix precedence -> precedence

fixityAssociativityOf :: Fixity -> Assoc
fixityAssociativityOf fixity = case fixity of
  Infix associativity _ -> associativity
  Prefix _ -> AssocNone
  Postfix _ -> AssocNone

applyOperator :: Span -> Name -> Bool -> Expr -> Expr -> Expr
applyOperator operatorSpan name isConstructor left right =
  let function =
        if isConstructor
          then ECon operatorSpan name
          else EVar operatorSpan name
      firstApplication =
        EApp (spanBetween (exprSpan left) (exprSpan function)) function left
   in EApp (spanBetween (exprSpan left) (exprSpan right)) firstApplication right

parseUnaryOrApplication :: Parser Expr
parseUnaryOrApplication = do
  kind <- peekKind
  case kind of
    TkOp "-" -> do
      nextKind <- peekAtKind 1
      if canStartExpr nextKind
        then do
          start <- currentStart
          operatorToken <- advance
          operand <- parseUnaryOrApplication
          end <- lastEnd
          pure
            ( EApp
                (mkSpan start end)
                (EVar (tokenSpan operatorToken) "negate")
                operand
            )
        else parseApplication
    _ -> parseApplication

parseApplication :: Parser Expr
parseApplication = do
  first <- parseAtom
  go first
  where
    go accumulated = do
      kind <- peekKind
      if canStartAtom kind
        then do
          next <- parseAtom
          let combined = EApp (spanBetween (exprSpan accumulated) (exprSpan next)) accumulated next
          go combined
        else pure accumulated

canStartExpr :: TokenKind -> Bool
canStartExpr kind = case kind of
  TkVar _ -> True
  TkCon _ -> True
  TkInt _ -> True
  TkChar _ -> True
  TkString _ -> True
  TkLParen -> True
  TkLBracket -> True
  TkLambda -> True
  TkKeyword "let" -> True
  TkKeyword "if" -> True
  TkKeyword "case" -> True
  TkKeyword "do" -> True
  TkOp "-" -> True
  _ -> False

canStartAtom :: TokenKind -> Bool
canStartAtom kind = case kind of
  TkVar _ -> True
  TkCon _ -> True
  TkInt _ -> True
  TkChar _ -> True
  TkString _ -> True
  TkLParen -> True
  TkLBracket -> True
  _ -> False

parseAtom :: Parser Expr
parseAtom = do
  kind <- peekKind
  case kind of
    TkVar name -> do
      token <- advance
      pure (EVar (tokenSpan token) name)
    TkCon name -> do
      token <- advance
      pure (ECon (tokenSpan token) name)
    TkInt value -> do
      token <- advance
      pure (ELit (tokenSpan token) (LInt value))
    TkChar value -> do
      token <- advance
      pure (ELit (tokenSpan token) (LChar value))
    TkString value -> do
      token <- advance
      pure (ELit (tokenSpan token) (LString value))
    TkLParen -> parseParenExpr
    TkLBracket -> parseListExpr
    _ -> failParse ("expected an expression, found " <> describeTokenKind kind)

parseParenExpr :: Parser Expr
parseParenExpr = do
  start <- currentStart
  _ <- expectKind (== TkLParen) "'('"
  kind <- peekKind
  case kind of
    TkRParen -> do
      token <- advance
      pure (ECon (mkSpan start (spanEnd (tokenSpan token))) "()")
    TkComma -> do
      name <- tupleConstructorName
      end <- lastEnd
      pure (ECon (mkSpan start end) name)
    TkOp raw -> do
      operatorToken <- advance
      let operator = stripBackticks raw
      next <- peekKind
      case next of
        TkRParen -> do
          endToken <- advance
          pure (ESection (mkSpan start (spanEnd (tokenSpan endToken))) (SectionName operator))
        _
          | operator == "-" -> do
              right <- parseExpression
              endToken <- expectKind (== TkRParen) "')'"
              pure
                ( EApp
                    (mkSpan start (spanEnd (tokenSpan endToken)))
                    (EVar (tokenSpan operatorToken) "negate")
                    right
                )
          | otherwise -> do
              right <- parseExpression
              endToken <- expectKind (== TkRParen) "')'"
              pure
                ( ESection
                    (mkSpan start (spanEnd (tokenSpan endToken)))
                    (SectionRight operator right)
                )
    _ -> do
      expression <- parseExpression
      next <- peekKind
      case next of
        TkComma -> do
          expressions <- parseTupleRemainder
          endToken <- expectKind (== TkRParen) "')' after a tuple"
          pure (ETuple (mkSpan start (spanEnd (tokenSpan endToken))) (expression : expressions))
        TkOp raw -> do
          operatorToken <- advance
          endToken <- expectKind (== TkRParen) "')' after an operator section"
          pure
            ( ESection
                (mkSpan start (spanEnd (tokenSpan endToken)))
                (SectionLeft expression (stripBackticks raw))
            )
        TkDoubleColon -> do
          _ <- advance
          type_ <- parseType
          endToken <- expectKind (== TkRParen) "')' after a type annotation"
          pure (EAnn (mkSpan start (spanEnd (tokenSpan endToken))) expression type_)
        TkRParen -> do
          endToken <- advance
          pure (setExprSpan (mkSpan start (spanEnd (tokenSpan endToken))) expression)
        _ -> failParse ("expected ')', ',', '::' or an operator in parentheses, found " <> describeTokenKind next)

parseTupleRemainder :: Parser [Expr]
parseTupleRemainder = do
  _ <- expectKind (== TkComma) "','"
  expression <- parseExpression
  next <- peekKind
  case next of
    TkComma -> (expression :) <$> parseTupleRemainder
    _ -> pure [expression]

tupleConstructorName :: Parser Name
tupleConstructorName = do
  commas <- countCommas 0
  _ <- expectKind (== TkRParen) "')' after a tuple constructor"
  pure ("(" <> replicate commas ',' <> ")")
  where
    countCommas count = do
      comma <- matchKind (== TkComma)
      case comma of
        Just _ -> countCommas (count + 1)
        Nothing -> pure count

parseListExpr :: Parser Expr
parseListExpr = do
  start <- currentStart
  _ <- expectKind (== TkLBracket) "'['"
  kind <- peekKind
  if kind == TkRBracket
    then do
      endToken <- advance
      pure (EList (mkSpan start (spanEnd (tokenSpan endToken))) [])
    else do
      first <- parseExpression
      next <- peekKind
      case next of
        TkPipe -> do
          _ <- advance
          statements <- parseComprehensionStatements
          endToken <- expectKind (== TkRBracket) "']' after a list comprehension"
          pure (EComp (mkSpan start (spanEnd (tokenSpan endToken))) first statements)
        TkDotDot -> do
          _ <- advance
          endKind <- peekKind
          case endKind of
            TkRBracket -> do
              endToken <- advance
              pure (ERange (mkSpan start (spanEnd (tokenSpan endToken))) first Nothing Nothing)
            _ -> do
              upper <- parseExpression
              endToken <- expectKind (== TkRBracket) "']' after a range"
              pure (ERange (mkSpan start (spanEnd (tokenSpan endToken))) first Nothing (Just upper))
        TkComma -> do
          _ <- advance
          second <- parseExpression
          nextKind <- peekKind
          case nextKind of
            TkDotDot -> do
              _ <- advance
              endKind <- peekKind
              case endKind of
                TkRBracket -> do
                  endToken <- advance
                  pure (ERange (mkSpan start (spanEnd (tokenSpan endToken))) first (Just second) Nothing)
                _ -> do
                  upper <- parseExpression
                  endToken <- expectKind (== TkRBracket) "']' after a range"
                  pure (ERange (mkSpan start (spanEnd (tokenSpan endToken))) first (Just second) (Just upper))
            TkRBracket -> do
              endToken <- advance
              pure (EList (mkSpan start (spanEnd (tokenSpan endToken))) [first, second])
            _ -> do
              rest <- parseListRemainder
              endToken <- expectKind (== TkRBracket) "']' after a list"
              pure (EList (mkSpan start (spanEnd (tokenSpan endToken))) (first : second : rest))
        TkRBracket -> do
          endToken <- advance
          pure (EList (mkSpan start (spanEnd (tokenSpan endToken))) [first])
        _ -> failParse ("expected ',', '..', '|' or ']' in a list, found " <> describeTokenKind next)

parseListRemainder :: Parser [Expr]
parseListRemainder = do
  _ <- expectKind (== TkComma) "','"
  expression <- parseExpression
  next <- peekKind
  case next of
    TkComma -> (expression :) <$> parseListRemainder
    _ -> pure [expression]

parseComprehensionStatements :: Parser [Stmt]
parseComprehensionStatements = do
  first <- parseComprehensionStatement
  rest <- many $ do
    _ <- expectKind (== TkComma) "',' between list comprehension statements"
    parseComprehensionStatement
  pure (first : rest)

parseComprehensionStatement :: Parser Stmt
parseComprehensionStatement = do
  start <- currentStart
  generator <- tryP $ do
    pattern_ <- parsePattern
    _ <- expectKind (== TkLArrow) "'<-' in a list comprehension generator"
    expr <- parseExpression
    pure (pattern_, expr)
  case generator of
    Just (pattern_, expr) -> do
      end <- lastEnd
      pure (Generator (mkSpan start end) pattern_ expr)
    Nothing -> do
      expr <- parseExpression
      end <- lastEnd
      pure (Qualifier (mkSpan start end) expr)

setExprSpan :: Span -> Expr -> Expr
setExprSpan span_ expression = case expression of
  EVar _ name -> EVar span_ name
  ECon _ name -> ECon span_ name
  ELit _ literal -> ELit span_ literal
  EApp _ function argument -> EApp span_ function argument
  ELam _ patterns body -> ELam span_ patterns body
  ELet _ declarations body -> ELet span_ declarations body
  EIf _ condition trueBranch falseBranch -> EIf span_ condition trueBranch falseBranch
  ECase _ scrutinee alternatives -> ECase span_ scrutinee alternatives
  EDo _ statements -> EDo span_ statements
  ETuple _ expressions -> ETuple span_ expressions
  EList _ expressions -> EList span_ expressions
  ERange _ fromExpression stepExpression toExpression -> ERange span_ fromExpression stepExpression toExpression
  EComp _ expression statements -> EComp span_ expression statements
  ESection _ section -> ESection span_ section
  EAnn _ expression type_ -> EAnn span_ expression type_

-- Patterns ------------------------------------------------------------------

parsePattern :: Parser Pattern
parsePattern = do
  first <- parsePatternApplication
  parsePatternRest 0 first

parsePatternApplication :: Parser Pattern
parsePatternApplication = do
  first <- parseAPat
  case first of
    PCon constructorspan name arguments -> do
      extra <- manyWhile startsAPat parseAPat
      pure (PCon constructorspan name (arguments <> extra))
    _ -> do
      kind <- peekKind
      if startsAPat kind
        then failParse "only constructors can be applied in a pattern"
        else pure first

parsePatternRest :: Int -> Pattern -> Parser Pattern
parsePatternRest minimumPrecedence left = do
  kind <- peekKind
  case kind of
    TkOp raw -> do
      let name = stripBackticks raw
      if not (isConstructorName name)
        then pure left
        else do
          fixity <- lookupFixityFor name
          let precedence = fixityPrecedenceOf fixity
          if precedence < minimumPrecedence
            then pure left
            else do
              operatorToken <- advance
              right <- parsePatternApplication
              let nextMinimum =
                    case fixityAssociativityOf fixity of
                      AssocRight -> precedence
                      _ -> precedence + 1
              right' <- parsePatternRest nextMinimum right
              parsePatternRest minimumPrecedence (PCon (tokenSpan operatorToken) name [left, right'])
    _ -> pure left
  where
    lookupFixityFor name = Parser $ \state -> Right (lookupFixity (psFixities state) name, state)

startsAPat :: TokenKind -> Bool
startsAPat kind = case kind of
  TkVar _ -> True
  TkCon _ -> True
  TkInt _ -> True
  TkChar _ -> True
  TkLParen -> True
  TkLBracket -> True
  _ -> False

parseAPat :: Parser Pattern
parseAPat = do
  kind <- peekKind
  case kind of
    TkVar name -> do
      token <- advance
      atToken <- matchKind (== TkAt)
      case atToken of
        Just _ -> do
          inner <- parseAPat
          pure (PAs (tokenSpan token) name inner)
        Nothing -> pure (PVar (tokenSpan token) name)
    TkUnderscore -> do
      token <- advance
      pure (PWild (tokenSpan token))
    TkCon name -> do
      token <- advance
      pure (PCon (tokenSpan token) name [])
    TkInt value -> do
      token <- advance
      pure (PLit (tokenSpan token) (LInt value))
    TkChar value -> do
      token <- advance
      pure (PLit (tokenSpan token) (LChar value))
    TkLParen -> do
      start <- currentStart
      _ <- advance
      next <- peekKind
      case next of
        TkRParen -> do
          token <- advance
          pure (PCon (mkSpan start (spanEnd (tokenSpan token))) "()" [])
        _ -> do
          first <- parsePattern
          afterFirst <- peekKind
          case afterFirst of
            TkComma -> do
              patterns <- parsePatternTupleRemainder
              endToken <- expectKind (== TkRParen) "')' after a tuple pattern"
              pure (PTuple (mkSpan start (spanEnd (tokenSpan endToken))) (first : patterns))
            TkRParen -> do
              endToken <- advance
              pure (setPatternSpan (mkSpan start (spanEnd (tokenSpan endToken))) first)
            _ -> failParse ("expected ',' or ')' in a parenthesized pattern, found " <> describeTokenKind afterFirst)
    TkLBracket -> do
      start <- currentStart
      _ <- advance
      next <- peekKind
      case next of
        TkRBracket -> do
          token <- advance
          pure (PList (mkSpan start (spanEnd (tokenSpan token))) [])
        _ -> do
          patterns <- parsePatternListRemainder
          endToken <- expectKind (== TkRBracket) "']' after a list pattern"
          pure (PList (mkSpan start (spanEnd (tokenSpan endToken))) patterns)
    _ -> failParse ("expected a pattern, found " <> describeTokenKind kind)

parsePatternTupleRemainder :: Parser [Pattern]
parsePatternTupleRemainder = do
  _ <- expectKind (== TkComma) "','"
  pattern_ <- parsePattern
  next <- peekKind
  case next of
    TkComma -> (pattern_ :) <$> parsePatternTupleRemainder
    _ -> pure [pattern_]

parsePatternListRemainder :: Parser [Pattern]
parsePatternListRemainder = do
  first <- parsePattern
  rest <- many $ do
    _ <- expectKind (== TkComma) "',' between list pattern elements"
    parsePattern
  pure (first : rest)

setPatternSpan :: Span -> Pattern -> Pattern
setPatternSpan span_ pattern_ = case pattern_ of
  PVar _ name -> PVar span_ name
  PWild _ -> PWild span_
  PLit _ literal -> PLit span_ literal
  PCon _ name patterns -> PCon span_ name patterns
  PTuple _ patterns -> PTuple span_ patterns
  PList _ patterns -> PList span_ patterns
  PAs _ name inner -> PAs span_ name inner
  PAnn _ inner type_ -> PAnn span_ inner type_

-- Types ---------------------------------------------------------------------

parseType :: Parser Type
parseType = do
  kind <- peekKind
  case kind of
    TkKeyword "forall" -> parseForallType
    _ -> do
      first <- parseTypeNoContext
      next <- peekKind
      case next of
        TkDArrow -> do
          _ <- advance
          predicates <- case typeToPredicates first of
            Left message -> failParse message
            Right result -> pure result
          rest <- parseType
          pure (TyContext (spanBetween (typeSpan first) (typeSpan rest)) predicates rest)
        _ -> pure first

parseForallType :: Parser Type
parseForallType = do
  start <- currentStart
  _ <- expectKeyword "forall"
  variables <- many1 expectVarName
  _ <- expectKind (== TkOp ".") "'.' after forall variables"
  body <- parseType
  end <- lastEnd
  pure (TyForall (mkSpan start end) variables body)

parseTypeNoContext :: Parser Type
parseTypeNoContext = do
  left <- parseBType
  next <- peekKind
  case next of
    TkArrow -> do
      _ <- advance
      right <- parseType
      pure (TyFun (spanBetween (typeSpan left) (typeSpan right)) left right)
    _ -> pure left

parseBType :: Parser Type
parseBType = do
  first <- parseAType
  rest <- manyWhile startsTypeAtom parseAType
  pure (foldl' (\function argument -> TyApp (spanBetween (typeSpan function) (typeSpan argument)) function argument) first rest)

startsTypeAtom :: TokenKind -> Bool
startsTypeAtom kind = case kind of
  TkCon _ -> True
  TkVar _ -> True
  TkLParen -> True
  TkLBracket -> True
  TkKeyword "forall" -> True
  _ -> False

parseAType :: Parser Type
parseAType = do
  kind <- peekKind
  case kind of
    TkCon name -> do
      token <- advance
      pure (TyCon (tokenSpan token) name)
    TkVar name -> do
      token <- advance
      pure (TyVar (tokenSpan token) name)
    TkLBracket -> do
      start <- currentStart
      _ <- advance
      type_ <- parseType
      endToken <- expectKind (== TkRBracket) "']' after a list type"
      pure (TyList (mkSpan start (spanEnd (tokenSpan endToken))) type_)
    TkLParen -> do
      start <- currentStart
      _ <- advance
      next <- peekKind
      case next of
        TkRParen -> do
          token <- advance
          pure (TyCon (mkSpan start (spanEnd (tokenSpan token))) "()")
        TkComma -> do
          commas <- countTypeCommas 0
          endToken <- expectKind (== TkRParen) "')' after a tuple type constructor"
          pure (TyCon (mkSpan start (spanEnd (tokenSpan endToken))) ("(" <> replicate commas ',' <> ")"))
        TkArrow -> do
          _ <- advance
          endToken <- expectKind (== TkRParen) "')' after '(->)'"
          pure (TyCon (mkSpan start (spanEnd (tokenSpan endToken))) "(->)")
        _ -> do
          first <- parseType
          afterFirst <- peekKind
          case afterFirst of
            TkComma -> do
              rest <- parseTypeTupleRemainder
              endToken <- expectKind (== TkRParen) "')' after a tuple type"
              pure (TyTuple (mkSpan start (spanEnd (tokenSpan endToken))) (first : rest))
            TkRParen -> do
              endToken <- advance
              pure (setTypeSpan (mkSpan start (spanEnd (tokenSpan endToken))) first)
            _ -> failParse ("expected ',' or ')' in a parenthesized type, found " <> describeTokenKind afterFirst)
    TkKeyword "forall" -> parseForallType
    _ -> failParse ("expected a type, found " <> describeTokenKind kind)

countTypeCommas :: Int -> Parser Int
countTypeCommas count = do
  comma <- matchKind (== TkComma)
  case comma of
    Just _ -> countTypeCommas (count + 1)
    Nothing -> pure count

parseTypeTupleRemainder :: Parser [Type]
parseTypeTupleRemainder = do
  _ <- expectKind (== TkComma) "','"
  type_ <- parseType
  next <- peekKind
  case next of
    TkComma -> (type_ :) <$> parseTypeTupleRemainder
    _ -> pure [type_]

setTypeSpan :: Span -> Type -> Type
setTypeSpan span_ type_ = case type_ of
  TyCon _ name -> TyCon span_ name
  TyVar _ name -> TyVar span_ name
  TyApp _ function argument -> TyApp span_ function argument
  TyFun _ argument result -> TyFun span_ argument result
  TyForall _ variables body -> TyForall span_ variables body
  TyContext _ predicates body -> TyContext span_ predicates body
  TyTuple _ types -> TyTuple span_ types
  TyList _ element -> TyList span_ element

typeToPredicates :: Type -> Either String [Pred]
typeToPredicates type_ = case type_ of
  TyApp _ (TyCon _ className) argument
    | startsWithUppercase className ->
        Right [Pred (spanBetween (typeSpan type_) (typeSpan argument)) className argument]
  TyTuple _ types -> concat <$> mapM typeToPredicates types
  _ -> Left "expected a class constraint such as 'Eq a' or '(Eq a, Show a)'"
  where
    startsWithUppercase [] = False
    startsWithUppercase (character : _) = isUpper character

peekAtKind :: Int -> Parser TokenKind
peekAtKind index = tokenKind <$> peekAt index
