-- | Interactive shell and file runner.
module WYAH.Repl
  ( RunResult(..)
  , runSource
  , runFile
  , compileSource
  , compileDetailed
  , runCompiled
  , repl
  , renderRunResult
  ) where

import Control.Exception (IOException, try)
import Data.List (isPrefixOf)
import System.IO (hFlush, isEOF, stdout)
import WYAH.Core (CoreModule(..), CoreExpr(..), prettyCore)
import WYAH.Desugar (desugarModule)
import WYAH.Elaborate (elaborateModule)
import WYAH.Eval (evalWithModule, renderValue)
import WYAH.Infer
import WYAH.Kind (CheckedModule, checkModule, kindOfTypeText)
import WYAH.Parser (parseModule, parseReplInput, parseTypeText)
import WYAH.Renamer
import WYAH.Renamed
import WYAH.Span (Span(..))
import WYAH.Type

data RunResult = RunResult
  { resultTypes :: [(String, String)]
  , resultType :: Maybe String
  , resultValue :: Maybe String
  , resultCore :: Maybe String
  }
  deriving (Eq, Show)

compileSource :: FilePath -> String -> Either String CoreModule
compileSource file source = do
  parsed <- either (Left . show) Right (parseModule file source)
  renamed <- either (Left . show) Right (renameModule file parsed)
  let desugared = desugarModule renamed
  checked <- either (Left . show) Right (checkModule file desugared)
  inferred <- either (Left . show) Right (inferModule file checked)
  either (Left . show) Right (elaborateModule checked inferred)

runSource :: String -> IO (Either String RunResult)
runSource source =
  case parseReplInput "<repl>" source of
    Left err -> pure (Left (show err))
    Right (Left _expression) -> do
      let synthetic = "__repl_value = " <> source <> "\n"
      case compileDetailed "<repl>" synthetic of
        Left err -> pure (Left err)
        Right (_, inferred, core) ->
          case [name | (name, _) <- inferredBindSchemes inferred, resolvedOriginal name == "__repl_value"] of
            [] -> pure (Left "internal REPL error: missing synthetic binding")
            name : _ -> do
              let scheme_ = maybe (Forall [] [] (TCon (TyCon "()" KStar 0))) id (lookup name (inferredBindSchemes inferred))
              evaluated <- evalWithModule "<repl>" core (CVar name)
              case evaluated of
                Left err -> pure (Left (show err))
                Right value ->
                  pure
                    ( Right
                        RunResult
                          { resultTypes = []
                          , resultType = Just (prettyScheme scheme_)
                          , resultValue = Just (renderValue core value)
                          , resultCore = Just (prettyCore (CVar name))
                          }
                    )
    Right (Right _) ->
      case compileDetailed "<repl>" source of
        Left err -> pure (Left err)
        Right (_, inferred, _) ->
          let schemes = [(resolvedOriginal name, prettyScheme scheme_) | (name, scheme_) <- inferredBindSchemes inferred]
           in pure (Right (RunResult schemes Nothing Nothing Nothing))

-- | Compile and retain the inferred schemes so the REPL can print types.
compileDetailed :: FilePath -> String -> Either String (CheckedModule, InferredModule, CoreModule)
compileDetailed file source = do
  parsed <- either (Left . show) Right (parseModule file source)
  renamed <- either (Left . show) Right (renameModule file parsed)
  let desugared = desugarModule renamed
  checked <- either (Left . show) Right (checkModule file desugared)
  inferred <- either (Left . show) Right (inferModule file checked)
  core <- either (Left . show) Right (elaborateModule checked inferred)
  pure (checked, inferred, core)

runCompiled :: FilePath -> (CheckedModule, InferredModule, CoreModule) -> IO (Either String RunResult)
runCompiled file (_, inferred, core) = do
  let schemes = [(resolvedOriginal name, prettyScheme scheme_) | (name, scheme_) <- inferredBindSchemes inferred]
  case [name | (name, _) <- coreBindings core, resolvedOriginal name == "main"] of
    [] ->
      pure (Right (RunResult schemes Nothing Nothing Nothing))
    name : _ -> do
      evaluated <- evalWithModule file core (CVar name)
      case evaluated of
        Left err -> pure (Left (show err))
        Right value ->
          pure
            ( Right
                RunResult
                  { resultTypes = schemes
                  , resultType = lookup "main" schemes
                  , resultValue = Just (renderValue core value)
                  , resultCore = Nothing
                  }
            )

runFile :: FilePath -> IO (Either String RunResult)
runFile file = do
  contents <- try (readFile file) :: IO (Either IOException String)
  case contents of
    Left err -> pure (Left (show err))
    Right source ->
      case compileDetailed file source of
        Left err -> pure (Left err)
        Right compiled -> runCompiled file compiled

renderRunResult :: RunResult -> String
renderRunResult result =
  let typeLine = maybe "" (\type_ -> "type: " <> type_) (resultType result)
      valueLine = maybe "" (\value -> "value: " <> value) (resultValue result)
      browseLines = [name <> " :: " <> scheme_ | (name, scheme_) <- resultTypes result]
   in unlines (filter (not . null) ([typeLine, valueLine] <> browseLines))

declarationSource :: ReplState -> String
declarationSource state
  | null (replDeclarations state) = "dummy = 0\n"
  | otherwise = replDeclarations state

-- REPL ----------------------------------------------------------------------

data ReplState = ReplState
  { replDeclarations :: String
  , replLoadedFile :: Maybe FilePath
  }

repl :: IO ()
repl = do
  putStrLn "ProtoHaskell Compiler 0.1.0 (:help for commands, :quit to exit)"
  loop (ReplState "" Nothing)

loop :: ReplState -> IO ()
loop state = do
  eof <- isEOF
  if eof
    then putStrLn "bye"
    else do
      putStr "> "
      hFlush stdout
      input <- getLine
      case words input of
        [] -> loop state
        (command : rest)
          | ":" `isPrefixOf` command -> do
              newState <- handleCommand state command rest
              case newState of
                Nothing -> pure ()
                Just state' -> loop state'
        _ -> do
          newState <- handleInput state input
          loop newState

handleCommand :: ReplState -> String -> [String] -> IO (Maybe ReplState)
handleCommand state command arguments = case command of
  ":quit" -> pure Nothing
  ":q" -> pure Nothing
  ":help" -> do
    putStrLn ":browse             show inferred types"
    putStrLn ":load <file>        load a module"
    putStrLn ":reload             reload the active module"
    putStrLn ":type <expr>        infer the type of an expression"
    putStrLn ":kind <type>        show the kind of a type"
    putStrLn ":core <expr>        show the elaborated Core of an expression"
    putStrLn ":source             show accumulated declarations"
    putStrLn ":module             show the interactive module header"
    putStrLn ":quit               exit"
    pure (Just state)
  ":browse" ->
    if null (replDeclarations state)
      then pure (Just state)
      else do
        case compileDetailed "<repl>" (replDeclarations state) of
          Left err -> putStrLn err
          Right (_, inferred, _) ->
            mapM_
              (\(name, scheme_) -> putStrLn (resolvedOriginal name <> " :: " <> prettyScheme scheme_))
              (inferredBindSchemes inferred)
        pure (Just state)
  ":load" -> case arguments of
    [] -> putStrLn "usage: :load <file>" >> pure (Just state)
    (file : _) -> do
      contents <- try (readFile file) :: IO (Either IOException String)
      case contents of
        Left err -> putStrLn (show err) >> pure (Just state)
        Right source -> do
          reportCompile source
          pure (Just state {replDeclarations = source, replLoadedFile = Just file})
  ":reload" -> case replLoadedFile state of
    Nothing -> do
      reportCompile (replDeclarations state)
      pure (Just state)
    Just file -> do
      contents <- try (readFile file) :: IO (Either IOException String)
      case contents of
        Left err -> putStrLn (show err) >> pure (Just state)
        Right source -> reportCompile source >> pure (Just state {replDeclarations = source})
  ":type" -> case unwords arguments of
    "" -> putStrLn "usage: :type <expr>" >> pure (Just state)
    expression -> do
      reportType state expression
      pure (Just state)
  ":kind" -> case unwords arguments of
    "" -> putStrLn "usage: :kind <type>" >> pure (Just state)
    typeText -> do
      reportKind state typeText
      pure (Just state)
  ":core" -> case unwords arguments of
    "" -> putStrLn "usage: :core <expr>" >> pure (Just state)
    expression -> do
      reportCore state expression
      pure (Just state)
  ":source" -> putStr (replDeclarations state) >> pure (Just state)
  ":module" -> putStrLn "module <interactive> where" >> pure (Just state)
  ":set" -> putStrLn "flags are not configurable in this build" >> pure (Just state)
  ":unset" -> putStrLn "flags are not configurable in this build" >> pure (Just state)
  _ -> putStrLn ("unknown command: " <> command) >> pure (Just state)
  where
    reportCompile source
      | null source = pure ()
      | otherwise = case compileDetailed "<repl>" source of
          Left err -> putStrLn err
          Right (_, inferred, _) ->
            mapM_
              (\(name, scheme_) -> putStrLn (resolvedOriginal name <> " :: " <> prettyScheme scheme_))
              (inferredBindSchemes inferred)

handleInput :: ReplState -> String -> IO ReplState
handleInput state input =
  case parseReplInput "<repl>" input of
    Left err -> putStrLn (show err) >> pure state
    Right (Right _) -> do
      let declarations = replDeclarations state <> input <> "\n"
      case compileDetailed "<repl>" declarations of
        Left err -> putStrLn err >> pure state
        Right (_, inferred, _) -> do
          mapM_
            (\(name, scheme_) -> putStrLn (resolvedOriginal name <> " :: " <> prettyScheme scheme_))
            (inferredBindSchemes inferred)
          pure state {replDeclarations = declarations}
    Right (Left _expression) -> do
      let synthetic = replDeclarations state <> "__repl_value = " <> input <> "\n"
      case compileDetailed "<repl>" synthetic of
        Left err -> putStrLn err >> pure state
        Right (_, inferred, core) ->
          case [name | (name, _) <- inferredBindSchemes inferred, resolvedOriginal name == "__repl_value"] of
            [] -> putStrLn "internal REPL error: missing binding" >> pure state
            name : _ -> do
              let scheme_ = maybe (Forall [] [] (TCon (TyCon "()" KStar 0))) id (lookup name (inferredBindSchemes inferred))
              putStrLn ("type: " <> prettyScheme scheme_)
              result <- evalWithModule "<repl>" core (CVar name)
              case result of
                Left err -> putStrLn (show err)
                Right value -> putStrLn ("value: " <> renderValue core value)
              pure state

reportType :: ReplState -> String -> IO ()
reportType state expression = do
  let synthetic = replDeclarations state <> "__repl_value = " <> expression <> "\n"
  case compileDetailed "<repl>" synthetic of
    Left err -> putStrLn err
    Right (_, inferred, _) ->
      case [scheme_ | (name, scheme_) <- inferredBindSchemes inferred, resolvedOriginal name == "__repl_value"] of
        scheme_ : _ -> putStrLn (prettyScheme scheme_)
        [] -> putStrLn "could not infer a type"

reportKind :: ReplState -> String -> IO ()
reportKind state typeText =
  case compileChecked state of
    Left err -> putStrLn err
    Right checked ->
      case parseTypeText "<repl>" typeText of
        Left err -> putStrLn (show err)
        Right surfaceType ->
          case kindOfTypeText "<repl>" checked surfaceType of
            Left err -> putStrLn (show err)
            Right kind -> putStrLn (prettyKind kind)

compileChecked :: ReplState -> Either String CheckedModule
compileChecked state
  | null (replDeclarations state) =
      case checkModule "<repl>" (RModule NoSpan Nothing []) of
        Left err -> Left (show err)
        Right checked -> Right checked
  | otherwise =
      case compileDetailed "<repl>" (replDeclarations state) of
        Left err -> Left err
        Right (checked, _, _) -> Right checked

reportCore :: ReplState -> String -> IO ()
reportCore state expression = do
  let synthetic = replDeclarations state <> "__repl_value = " <> expression <> "\n"
      trimmed = unwords (words expression)
  case compileDetailed "<repl>" synthetic of
    Left err -> putStrLn err
    Right (_, _, core) ->
      case [body | (name, body) <- coreBindings core, resolvedOriginal name == trimmed] of
        body : _ -> putStrLn (trimmed <> " = " <> prettyCore body)
        [] ->
          case [body | (name, body) <- coreBindings core, resolvedOriginal name == "__repl_value"] of
            body : _ -> putStrLn (prettyCore body)
            [] -> putStrLn "could not elaborate an expression"
