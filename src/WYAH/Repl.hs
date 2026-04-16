module WYAH.Repl
  ( repl
  , runSource
  ) where

import System.IO (hFlush, stdout)
import WYAH.Core (toCore)
import WYAH.Eval (evalExpr)
import WYAH.Infer (inferProgram)
import WYAH.Name (lowerProgram, renameProgram)
import WYAH.Parser (parseProgram)

runSource :: String -> Either String String
runSource input = do
  program <- firstShow (parseProgram "<repl>" input)
  renamedProgram <- firstShow (renameProgram program)
  inferred <- firstShow (inferProgram renamedProgram)
  value <- firstShow (evalExpr (toCore (lowerProgram renamedProgram)))
  pure ("type: " <> show inferred <> "\nvalue: " <> show value)

repl :: IO ()
repl = do
  putStrLn "WYAH scaffold REPL (:quit to exit)"
  loop
  where
    loop = do
      putStr "> "
      hFlush stdout
      input <- getLine
      if input == ":quit"
        then putStrLn "bye"
        else do
          case runSource input of
            Left err -> putStrLn err
            Right out -> putStrLn out
          loop

firstShow :: Show e => Either e a -> Either String a
firstShow = either (Left . show) Right
