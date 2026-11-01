-- | Tokenizer and layout algorithm.
--
-- The tokenizer is handwritten on purpose: the first half of the book is
-- about understanding parsing mechanics, and a small lexer makes the layout
-- rule explicit.  The layout pass inserts virtual braces and semicolons so
-- that the parser proper can be written as if every block were explicit.
module WYAH.Lexer
  ( Token(..)
  , TokenKind(..)
  , LayoutMode(..)
  , LexError(..)
  , lexTokens
  , lexAndLayout
  , layoutTokens
  , isKeyword
  , describeToken
  ) where

import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace, isUpper)
import WYAH.Diagnostic
import WYAH.Span

data Token = Token
  { tokenSpan :: Span
  , tokenKind :: TokenKind
  }
  deriving (Eq, Show)

data TokenKind
  = TkVar String
  | TkCon String
  | TkInt Integer
  | TkChar Char
  | TkString String
  | TkOp String
  | TkKeyword String
  | TkLParen
  | TkRParen
  | TkLBracket
  | TkRBracket
  | TkLBrace
  | TkRBrace
  | TkComma
  | TkSemi
  | TkDoubleColon
  | TkEquals
  | TkPipe
  | TkArrow
  | TkLArrow
  | TkDArrow
  | TkLambda
  | TkAt
  | TkUnderscore
  | TkDotDot
  | TkEOF
  deriving (Eq, Show)

isKeyword :: String -> TokenKind -> Bool
isKeyword keyword kind = kind == TkKeyword keyword

data LayoutMode
  = LayoutModule
  | LayoutExpression
  deriving (Eq, Show)

data LexError = LexError
  { lexErrorSource :: FilePath
  , lexErrorSpan :: Span
  , lexErrorMessage :: String
  }
  deriving (Eq)

instance Show LexError where
  show err =
    renderDiagnostic
      Diagnostic
        { diagnosticPhase = phaseName ParsePhase
        , diagnosticSource = Just (lexErrorSource err)
        , diagnosticSpan = Just (lexErrorSpan err)
        , diagnosticMessage = lexErrorMessage err
        }

lexTokens :: FilePath -> String -> Either LexError [Token]
lexTokens file input = go startOfInput input []
  where
    go position [] acc = Right (reverse acc)
    go position chars acc
      | (space, rest) <- span isSpace chars
      , not (null space) =
          go (foldl advancePosition position space) rest acc
      | '-' : '-' : rest <- chars =
          let (comment, remaining) = break (== '\n') rest
              consumed = "--" <> comment
          in go (foldl advancePosition position consumed) remaining acc
      | '{' : '-' : rest <- chars = do
          (remaining, endPosition) <- skipBlockComment file position rest
          go endPosition remaining acc
      | char : rest <- chars
      , char == '\'' = do
          (value, remaining) <- lexChar file position rest
          let consumed = take (length chars - length remaining) chars
              endPosition = foldl advancePosition position consumed
          go endPosition remaining (Token (Span position endPosition) (TkChar value) : acc)
      | char : rest <- chars
      , char == '"' = do
          (value, remaining) <- lexString file position rest
          let consumed = take (length chars - length remaining) chars
              endPosition = foldl advancePosition position consumed
          go endPosition remaining (Token (Span position endPosition) (TkString value) : acc)
      | char : rest <- chars
      , char == '`' =
          case break (== '`') rest of
            (name, '`' : remaining)
              | not (null name)
              , all isIdentRest name ->
                  let lexeme = '`' : name <> "`"
                      endPosition = foldl advancePosition position lexeme
                   in go
                        endPosition
                        remaining
                        (Token (Span position endPosition) (TkOp lexeme) : acc)
            _ ->
              Left
                ( LexError
                    file
                    (Span position position)
                    "unterminated backtick operator"
                )
      | char : rest <- chars
      , isDigit char =
          let (digits, remaining) = span isDigit rest
              lexeme = char : digits
              endPosition = foldl advancePosition position lexeme
              value = read lexeme
           in go endPosition remaining (Token (Span position endPosition) (TkInt value) : acc)
      | char : rest <- chars
      , isIdentInitial char =
          let (suffix, remaining) = span isIdentRest rest
              name = char : suffix
              endPosition = foldl advancePosition position name
              kind = classifyWord name
           in go endPosition remaining (Token (Span position endPosition) kind : acc)
      | char : rest <- chars
      , isSymbolChar char =
          let (suffix, remaining) = span isSymbolChar rest
              lexeme = char : suffix
              endPosition = foldl advancePosition position lexeme
           in go endPosition remaining (Token (Span position endPosition) (classifySymbol lexeme) : acc)
      | char : rest <- chars
      , char `elem` ("()[]{};, " :: String) =
          let endPosition = advancePosition position char
              kind = case char of
                '(' -> TkLParen
                ')' -> TkRParen
                '[' -> TkLBracket
                ']' -> TkRBracket
                '{' -> TkLBrace
                '}' -> TkRBrace
                ',' -> TkComma
                ';' -> TkSemi
                _ -> TkOp [char]
           in go endPosition rest (Token (Span position endPosition) kind : acc)
      | char : rest <- chars =
          Left
            ( LexError
                file
                (Span position (advancePosition position char))
                ("unexpected character " <> show char)
            )

skipBlockComment :: FilePath -> Position -> String -> Either LexError (String, Position)
skipBlockComment file start = go start 1
  where
    go position depth chars = case chars of
      [] ->
        Left
          ( LexError
              file
              (Span start position)
              "unterminated block comment"
          )
      '-' : '}' : rest
        | depth == 1 -> Right (rest, advancePosition (advancePosition position '-') '}')
        | otherwise -> go (advancePosition (advancePosition position '-') '}') (depth - 1) rest
      '{' : '-' : rest ->
        go (advancePosition (advancePosition position '{') '-') (depth + 1) rest
      char : rest -> go (advancePosition position char) depth rest

lexChar :: FilePath -> Position -> String -> Either LexError (Char, String)
lexChar file start chars = case chars of
  [] -> bad "unterminated character literal"
  '\\' : rest -> do
    (value, remaining) <- escape file start rest
    case remaining of
      '\'' : after -> Right (value, after)
      _ -> bad "character literal is missing its closing quote"
  char : '\'' : rest
    | char /= '\n' -> Right (char, rest)
  _ -> bad "character literal must contain exactly one character"
  where
    bad message = Left (LexError file (Span start start) message)

lexString :: FilePath -> Position -> String -> Either LexError (String, String)
lexString file start = go []
  where
    go _ [] = Left (LexError file (Span start start) "unterminated string literal")
    go acc ('"' : rest) = Right (reverse acc, rest)
    go acc ('\\' : rest) = do
      (value, remaining) <- escape file start rest
      go (value : acc) remaining
    go acc (char : rest)
      | char == '\n' = Left (LexError file (Span start start) "newline in string literal")
      | otherwise = go (char : acc) rest

escape :: FilePath -> Position -> String -> Either LexError (Char, String)
escape file start chars = case chars of
  [] -> bad "unterminated escape sequence"
  ('n' : rest) -> Right ('\n', rest)
  ('t' : rest) -> Right ('\t', rest)
  ('r' : rest) -> Right ('\r', rest)
  ('\\' : rest) -> Right ('\\', rest)
  ('\'' : rest) -> Right ('\'', rest)
  ('"' : rest) -> Right ('"', rest)
  (char : rest)
    | isDigit char ->
        let (digits, remaining) = span isDigit (char : rest)
         in Right (toEnum (read digits), remaining)
  (char : _) -> bad ("unknown escape sequence \\" <> [char])
  where
    bad message = Left (LexError file (Span start start) message)

isIdentInitial :: Char -> Bool
isIdentInitial char = isAlpha char || char == '_'

isIdentRest :: Char -> Bool
isIdentRest char = isAlphaNum char || char == '_' || char == '\''

isSymbolChar :: Char -> Bool
isSymbolChar char = char `elem` ("!#$%&*+./<=>?@\\^|-~:" :: String)

classifyWord :: String -> TokenKind
classifyWord name
  | name `elem` reservedWords = TkKeyword name
  | name == "_" = TkUnderscore
  | isUpper (head name) = TkCon name
  | otherwise = TkVar name
  where
    reservedWords =
      [ "module"
      , "where"
      , "data"
      , "type"
      , "class"
      , "instance"
      , "deriving"
      , "infixl"
      , "infixr"
      , "infix"
      , "let"
      , "in"
      , "if"
      , "then"
      , "else"
      , "case"
      , "of"
      , "do"
      , "forall"
      , "as"
      , "hiding"
      ]

classifySymbol :: String -> TokenKind
classifySymbol lexeme = case lexeme of
  "::" -> TkDoubleColon
  "=" -> TkEquals
  "|" -> TkPipe
  "->" -> TkArrow
  "<-" -> TkLArrow
  "=>" -> TkDArrow
  "\\" -> TkLambda
  "@" -> TkAt
  ".." -> TkDotDot
  _ -> TkOp lexeme

-- | Tokenize and insert layout braces/semicolons.
lexAndLayout :: LayoutMode -> FilePath -> String -> Either LexError [Token]
lexAndLayout mode file input = do
  tokens <- lexTokens file input
  layoutTokens mode file tokens

-- | Insert virtual braces and semicolons and append 'TkEOF'.
layoutTokens :: LayoutMode -> FilePath -> [Token] -> Either LexError [Token]
layoutTokens mode _file tokens =
  case tokens of
    [] -> Right [Token (Span startOfInput startOfInput) TkEOF]
    first : _ -> do
      let firstSpan = tokenSpan first
          startsWithModule = isKeyword "module" (tokenKind first)
          topContext =
            LayoutContext
              { lcIndent = positionColumn (spanStart firstSpan)
              , lcExplicit = False
              , lcKind = LayoutTop
              , lcJustOpened = True
              }
          (initialStack, prefix) =
            case mode of
              LayoutExpression -> ([], [])
              LayoutModule
                | startsWithModule -> ([], [])
                | otherwise ->
                    ( [topContext]
                    , [Token (Span (spanStart firstSpan) (spanStart firstSpan)) TkLBrace]
                    )
      let (body, endSpan) = layoutGo initialStack Nothing 0 [] tokens
      pure (prefix <> body <> [Token endSpan TkEOF])

data LayoutKind
  = LayoutTop
  | LayoutWhere
  | LayoutLet
  | LayoutOf
  | LayoutDo
  | LayoutExplicit
  deriving (Eq, Show)

data LayoutContext = LayoutContext
  { lcIndent :: Int
  , lcExplicit :: Bool
  , lcKind :: LayoutKind
  , lcJustOpened :: Bool
  }

layoutGo :: [LayoutContext] -> Maybe Int -> Int -> [Token] -> [Token] -> ([Token], Span)
layoutGo stack0 previousLine0 bracketDepth0 acc0 tokens0 = case tokens0 of
  [] ->
    let endSpan = case acc0 of
          [] -> Span startOfInput startOfInput
          _ -> tokenSpan (last acc0)
     in (acc0 <> closeAll stack0, endSpan)
  token : rest ->
    let currentSpan = tokenSpan token
        currentLine = positionLine (spanStart currentSpan)
        currentColumn = positionColumn (spanStart currentSpan)
        onNewLine = maybe False (/= currentLine) previousLine0
        kind = tokenKind token
        isWhere = kind == TkKeyword "where"
        isIn = kind == TkKeyword "in"
        (stackPre, accPre)
          | isWhere = closeDoOrOf stack0 acc0
          | otherwise = (stack0, acc0)
        (stack1, acc1)
          | onNewLine && bracketDepth0 == 0 && not isWhere && not isIn =
              newlineLayout currentColumn stackPre accPre
          | otherwise = (stackPre, accPre)
     in case kind of
          TkKeyword "in" ->
            case closeToLet stack1 acc1 currentSpan of
              Just (stack2, acc2) ->
                layoutGo stack2 (Just currentLine) bracketDepth0 (acc2 <> [token]) rest
              Nothing ->
                emitToken stack1 (Just currentLine) bracketDepth0 (acc1 <> [token]) rest
          _ ->
            let stackReset = resetJustOpened stack1
                bracketDepth1 = updateBracketDepth bracketDepth0 kind
             in case kind of
                  _
                    | isBlockKeyword kind
                    , not (nextIsExplicitBrace rest) ->
                        let nextSpan = case rest of
                              next : _ -> tokenSpan next
                              [] -> currentSpan
                            indent = positionColumn (spanStart nextSpan)
                            context =
                              LayoutContext
                                { lcIndent = indent
                                , lcExplicit = False
                                , lcKind = layoutKindFor kind
                                , lcJustOpened = True
                                }
                            openBrace =
                              Token
                                (Span (spanEnd currentSpan) (spanEnd currentSpan))
                                TkLBrace
                         in layoutGo
                              (context : stackReset)
                              (Just currentLine)
                              bracketDepth1
                              (acc1 <> [token, openBrace])
                              rest
                  TkLBrace ->
                    layoutGo
                      ( LayoutContext currentColumn True LayoutExplicit False : stackReset
                      )
                      (Just currentLine)
                      bracketDepth1
                      (acc1 <> [token])
                      rest
                  TkRBrace ->
                    case stackReset of
                      (context : restStack)
                        | lcExplicit context ->
                            layoutGo restStack (Just currentLine) bracketDepth1 (acc1 <> [token]) rest
                      _ ->
                        layoutGo stackReset (Just currentLine) bracketDepth1 (acc1 <> [token]) rest
                  _ ->
                    layoutGo stackReset (Just currentLine) bracketDepth1 (acc1 <> [token]) rest

emitToken :: [LayoutContext] -> Maybe Int -> Int -> [Token] -> [Token] -> ([Token], Span)
emitToken = layoutGo

newlineLayout :: Int -> [LayoutContext] -> [Token] -> ([LayoutContext], [Token])
newlineLayout column stack acc =
  let (stackAfter, accAfter) = closeDeeper column stack acc
      accWithSemi = case stackAfter of
        (context : _)
          | not (lcExplicit context)
          , not (lcJustOpened context)
          , column == lcIndent context ->
              accAfter <> [virtualSemi column]
        _ -> accAfter
   in (stackAfter, accWithSemi)

closeDeeper :: Int -> [LayoutContext] -> [Token] -> ([LayoutContext], [Token])
closeDeeper column stack acc = case stack of
  (context : rest)
    | not (lcExplicit context)
    , column < lcIndent context ->
        closeDeeper column rest (acc <> [virtualRBrace])
  _ -> (stack, acc)

-- | A @where@ clause belongs to the current declaration, not to an
-- expression block.  Close any @do@ or @of@ contexts opened on the right
-- hand side before the @where@ is emitted.
closeDoOrOf :: [LayoutContext] -> [Token] -> ([LayoutContext], [Token])
closeDoOrOf stack acc = case stack of
  (context : rest)
    | not (lcExplicit context)
    , lcKind context == LayoutDo || lcKind context == LayoutOf ->
        closeDoOrOf rest (acc <> [virtualRBrace])
  _ -> (stack, acc)

-- | Close implicit contexts until the enclosing implicit @let@ context is
-- closed, emitting its closing brace.  Returns 'Nothing' when @in@ is not
-- closing an implicit let block.
closeToLet :: [LayoutContext] -> [Token] -> Span -> Maybe ([LayoutContext], [Token])
closeToLet stack acc span_ = go stack acc
  where
    go [] _ = Nothing
    go (context : rest) acc'
      | lcExplicit context = Nothing
      | lcKind context == LayoutLet = Just (rest, acc' <> [virtualRBraceAt span_])
      | otherwise = go rest (acc' <> [virtualRBraceAt span_])

closeAll :: [LayoutContext] -> [Token]
closeAll stack = go stack []
  where
    go [] acc = acc
    go (context : rest) acc
      | lcExplicit context = acc
      | otherwise = go rest (acc <> [virtualRBrace])

resetJustOpened :: [LayoutContext] -> [LayoutContext]
resetJustOpened [] = []
resetJustOpened (context : rest)
  | lcJustOpened context = context {lcJustOpened = False} : rest
  | otherwise = context : rest

virtualSemi :: Int -> Token
virtualSemi _ = Token NoSpan TkSemi

virtualRBrace :: Token
virtualRBrace = Token NoSpan TkRBrace

virtualRBraceAt :: Span -> Token
virtualRBraceAt span_ =
  Token (Span (spanStart span_) (spanStart span_)) TkRBrace

updateBracketDepth :: Int -> TokenKind -> Int
updateBracketDepth depth kind = case kind of
  TkLParen -> depth + 1
  TkLBracket -> depth + 1
  TkRParen -> max 0 (depth - 1)
  TkRBracket -> max 0 (depth - 1)
  _ -> depth

positionLine :: Position -> Int
positionLine (Position (line, _)) = line

positionColumn :: Position -> Int
positionColumn (Position (_, column)) = column

isBlockKeyword :: TokenKind -> Bool
isBlockKeyword kind = kind `elem` [TkKeyword "let", TkKeyword "where", TkKeyword "of", TkKeyword "do"]

layoutKindFor :: TokenKind -> LayoutKind
layoutKindFor kind = case kind of
  TkKeyword "let" -> LayoutLet
  TkKeyword "where" -> LayoutWhere
  TkKeyword "of" -> LayoutOf
  TkKeyword "do" -> LayoutDo
  _ -> LayoutTop

nextIsExplicitBrace :: [Token] -> Bool
nextIsExplicitBrace (token : _) = tokenKind token == TkLBrace
nextIsExplicitBrace [] = False

describeToken :: Token -> String
describeToken token = case tokenKind token of
  TkVar name -> "identifier " <> name
  TkCon name -> "constructor " <> name
  TkInt n -> "integer " <> show n
  TkChar c -> "character " <> show c
  TkString s -> "string " <> show s
  TkOp op -> "operator " <> op
  TkKeyword keyword -> "keyword " <> keyword
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
