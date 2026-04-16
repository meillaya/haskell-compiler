module WYAH.Parser
  ( ParseError(..)
  , parseExpr
  , parseProgram
  ) where

import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import WYAH.Diagnostic
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
        { diagnosticPhase = "parse"
        , diagnosticSource = Just (parseErrorFile err)
        , diagnosticSpan = Just (parseErrorSpan err)
        , diagnosticMessage = parseErrorMessage err
        }

data Token
  = TokenLambda Span
  | TokenArrow Span
  | TokenEquals Span
  | TokenSemi Span
  | TokenLet Span
  | TokenIn Span
  | TokenIf Span
  | TokenThen Span
  | TokenElse Span
  | TokenLParen Span
  | TokenRParen Span
  | TokenInt Span Integer
  | TokenBool Span Bool
  | TokenIdent Span Name
  | TokenOp Span String
  deriving (Eq, Show)

tokenSpan :: Token -> Span
tokenSpan token = case token of
  TokenLambda sp -> sp
  TokenArrow sp -> sp
  TokenEquals sp -> sp
  TokenSemi sp -> sp
  TokenLet sp -> sp
  TokenIn sp -> sp
  TokenIf sp -> sp
  TokenThen sp -> sp
  TokenElse sp -> sp
  TokenLParen sp -> sp
  TokenRParen sp -> sp
  TokenInt sp _ -> sp
  TokenBool sp _ -> sp
  TokenIdent sp _ -> sp
  TokenOp sp _ -> sp

parseExpr :: FilePath -> String -> Either ParseError Expr
parseExpr file input = do
  tokens <- tokenize file input
  case parseTokens file tokens of
    Left err -> Left err
    Right (expr, []) -> Right expr
    Right (_, token : _) -> Left (unexpectedToken file token)

parseProgram :: FilePath -> String -> Either ParseError Program
parseProgram file input = do
  tokens <- tokenize file input
  case parseProgramTokens file tokens of
    Left err -> Left err
    Right (program, []) -> Right program
    Right (_, token : _) -> Left (unexpectedToken file token)

parseProgramTokens :: FilePath -> [Token] -> Either ParseError (Program, [Token])
parseProgramTokens file = go []
  where
    go decls tokens =
      case tokens of
        TokenIdent nameSpan binder : TokenEquals _ : rest -> do
          (boundExpr, remaining) <- parseTokens file rest
          case remaining of
            TokenSemi semiSpan : rest' ->
              if null rest'
                then Left (ParseError file semiSpan "expected final expression after top-level declarations")
                else do
                  let decl = Decl (spanBetween nameSpan semiSpan) binder boundExpr
                  go (decls <> [decl]) rest'
            [] -> Left (ParseError file nameSpan "expected ';' after top-level declaration")
            token : _ -> Left (ParseError file (tokenSpan token) "expected ';' after top-level declaration")
        _ -> do
          (finalExpr, remaining) <- parseTokens file tokens
          pure (Program decls finalExpr, remaining)

parseTokens :: FilePath -> [Token] -> Either ParseError (Expr, [Token])
parseTokens file tokens =
  case tokens of
    TokenLet _ : _ -> parseLet file tokens
    TokenIf _ : _ -> parseIf file tokens
    TokenLambda _ : _ -> parseLambda file tokens
    _ -> parseAdditive file tokens

parseLet :: FilePath -> [Token] -> Either ParseError (Expr, [Token])
parseLet file tokens =
  case tokens of
    TokenLet letSpan : TokenIdent _ binder : TokenEquals _ : rest -> do
      (boundExpr, afterBound) <- parseTokens file rest
      case afterBound of
        TokenIn _ : bodyTokens -> do
          (bodyExpr, remaining) <- parseTokens file bodyTokens
          let exprSp = spanBetween letSpan (exprSpan bodyExpr)
          pure (Let exprSp binder boundExpr bodyExpr, remaining)
        [] -> Left (ParseError file letSpan "expected `in` after let binding")
        token : _ -> Left (unexpectedToken file token)
    TokenLet letSpan : _ -> Left (ParseError file letSpan "expected `let <name> = <expr> in <expr>`")
    _ -> parseIf file tokens

parseIf :: FilePath -> [Token] -> Either ParseError (Expr, [Token])
parseIf file tokens =
  case tokens of
    TokenIf ifSpan : rest -> do
      (condExpr, afterCond) <- parseTokens file rest
      case afterCond of
        TokenThen _ : thenTokens -> do
          (thenExpr, afterThen) <- parseTokens file thenTokens
          case afterThen of
            TokenElse _ : elseTokens -> do
              (elseExpr, remaining) <- parseTokens file elseTokens
              let exprSp = spanBetween ifSpan (exprSpan elseExpr)
              pure (If exprSp condExpr thenExpr elseExpr, remaining)
            [] -> Left (ParseError file ifSpan "expected `else` branch")
            token : _ -> Left (unexpectedToken file token)
        [] -> Left (ParseError file ifSpan "expected `then` branch")
        token : _ -> Left (unexpectedToken file token)
    _ -> parseLambda file tokens

parseLambda :: FilePath -> [Token] -> Either ParseError (Expr, [Token])
parseLambda file tokens =
  case tokens of
    TokenLambda lambdaSpan : TokenIdent _ binder : TokenArrow _ : rest -> do
      (body, remaining) <- parseTokens file rest
      let exprSp = spanBetween lambdaSpan (exprSpan body)
      pure (Lam exprSp binder body, remaining)
    TokenLambda lambdaSpan : _ -> Left (ParseError file lambdaSpan "expected lambda binder followed by ->")
    _ -> parseAdditive file tokens

parseAdditive :: FilePath -> [Token] -> Either ParseError (Expr, [Token])
parseAdditive file tokens = do
  (firstTerm, rest) <- parseUnary file tokens
  parseMore firstTerm rest
  where
    parseMore acc remaining =
      case remaining of
        TokenOp sp "+" : rest -> do
          (rhs, rest') <- parseUnary file rest
          let combined = applyBinary sp "add" acc rhs
          parseMore combined rest'
        _ -> Right (acc, remaining)

parseUnary :: FilePath -> [Token] -> Either ParseError (Expr, [Token])
parseUnary file tokens =
  case tokens of
    TokenOp sp "+" : rest -> parseUnary file rest
    TokenOp sp "-" : rest -> do
      (expr, remaining) <- parseUnary file rest
      pure (applyUnary sp "pred" expr, remaining)
    _ -> parseApplication file tokens

parseApplication :: FilePath -> [Token] -> Either ParseError (Expr, [Token])
parseApplication file tokens = do
  (firstTerm, rest) <- parseAtom file tokens
  parseMoreApplications firstTerm rest
  where
    parseMoreApplications acc remaining =
      case remaining of
        [] -> Right (acc, [])
        TokenRParen _ : _ -> Right (acc, remaining)
        TokenIn _ : _ -> Right (acc, remaining)
        TokenThen _ : _ -> Right (acc, remaining)
        TokenElse _ : _ -> Right (acc, remaining)
        TokenSemi _ : _ -> Right (acc, remaining)
        TokenOp _ _ : _ -> Right (acc, remaining)
        _ ->
          case parseAtom file remaining of
            Left _ -> Right (acc, remaining)
            Right (nextTerm, rest') ->
              let exprSp = spanBetween (exprSpan acc) (exprSpan nextTerm)
               in parseMoreApplications (App exprSp acc nextTerm) rest'

parseAtom :: FilePath -> [Token] -> Either ParseError (Expr, [Token])
parseAtom file tokens =
  case tokens of
    TokenLParen openSpan : rest
      | null rest -> Left (ParseError file openSpan "expected expression after opening parenthesis")
      | otherwise -> do
          (inner, remaining) <-
            case parseTokens file rest of
              Left (ParseError errFile NoSpan errMsg) -> Left (ParseError errFile openSpan errMsg)
              other -> other
          case remaining of
            TokenRParen closeSpan : rest' ->
              pure (setExprSpan (spanBetween openSpan closeSpan) inner, rest')
            [] -> Left (ParseError file openSpan "expected closing parenthesis")
            token : _ -> Left (unexpectedToken file token)
    TokenInt sp value : rest -> Right (Lit sp (LitInt value), rest)
    TokenBool sp value : rest -> Right (Lit sp (LitBool value), rest)
    TokenIdent sp name : rest -> Right (Var sp name, rest)
    TokenLet _ : _ -> parseLet file tokens
    TokenIf _ : _ -> parseIf file tokens
    TokenLambda _ : _ -> parseLambda file tokens
    token : _ -> Left (unexpectedToken file token)
    [] -> Left (ParseError file NoSpan "unexpected end of input")

applyUnary :: Span -> Name -> Expr -> Expr
applyUnary sp name expr =
  let fun = Var sp name
      exprSp = spanBetween (exprSpan fun) (exprSpan expr)
   in App exprSp fun expr

applyBinary :: Span -> Name -> Expr -> Expr -> Expr
applyBinary sp name left right =
  let fun = Var sp name
      lhsApp = App (spanBetween (exprSpan fun) (exprSpan left)) fun left
   in App (spanBetween (exprSpan lhsApp) (exprSpan right)) lhsApp right

setExprSpan :: Span -> Expr -> Expr
setExprSpan sp expr = case expr of
  Var _ name -> Var sp name
  Lit _ lit -> Lit sp lit
  Lam _ binder body -> Lam sp binder body
  App _ fun arg -> App sp fun arg
  Let _ binder bound body -> Let sp binder bound body
  If _ cond yes no -> If sp cond yes no

tokenize :: FilePath -> String -> Either ParseError [Token]
tokenize file = go startOfInput
  where
    go _ [] = Right []
    go pos (char : rest)
      | isSpace char = go (advancePosition pos char) rest
      | char == '\\' = emit pos [char] TokenLambda rest
      | char == '(' = emit pos [char] TokenLParen rest
      | char == ')' = emit pos [char] TokenRParen rest
      | char == '=' = emit pos [char] TokenEquals rest
      | char == ';' = emit pos [char] TokenSemi rest
      | char == '+' = emit pos [char] (\sp -> TokenOp sp "+") rest
      | char == '-' && take 1 rest == ">" = emit pos "->" TokenArrow (drop 1 rest)
      | char == '-' = emit pos [char] (\sp -> TokenOp sp "-") rest
      | isDigit char =
          let (digits, remainder) = span isDigit rest
              lexeme = char : digits
           in emit pos lexeme (\sp -> TokenInt sp (read lexeme)) remainder
      | isInitial char =
          let (suffix, remainder) = span isIdentRest rest
              name = char : suffix
           in emit pos name (keywordOrIdent name) remainder
      | otherwise = Left (ParseError file (Span pos (advancePosition pos char)) ("unexpected character " <> show char))

    emit pos lexeme constructor remainder = do
      let endPos = foldl advancePosition pos lexeme
          current = constructor (Span pos endPos)
      (current :) <$> go endPos remainder

    isInitial c = isAlpha c || c == '_'
    isIdentRest c = isAlphaNum c || c `elem` ['_', '\'']

keywordOrIdent :: Name -> Span -> Token
keywordOrIdent name sp = case name of
  "let" -> TokenLet sp
  "in" -> TokenIn sp
  "if" -> TokenIf sp
  "then" -> TokenThen sp
  "else" -> TokenElse sp
  "true" -> TokenBool sp True
  "false" -> TokenBool sp False
  _ -> TokenIdent sp name

unexpectedToken :: FilePath -> Token -> ParseError
unexpectedToken file token = ParseError file (tokenSpan token) ("unexpected token " <> describeToken token)

describeToken :: Token -> String
describeToken token = case token of
  TokenLambda _ -> "\\"
  TokenArrow _ -> "->"
  TokenEquals _ -> "="
  TokenSemi _ -> ";"
  TokenLet _ -> "let"
  TokenIn _ -> "in"
  TokenIf _ -> "if"
  TokenThen _ -> "then"
  TokenElse _ -> "else"
  TokenLParen _ -> "("
  TokenRParen _ -> ")"
  TokenInt _ n -> show n
  TokenBool _ b -> if b then "true" else "false"
  TokenIdent _ name -> name
  TokenOp _ op -> op
