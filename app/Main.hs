module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import WYAH.Repl (renderRunResult, repl, runFile)

main :: IO ()
main = do
  arguments <- getArgs
  case arguments of
    [] -> repl
    ["--help"] -> putStrLn usage
    ["-h"] -> putStrLn usage
    [file] -> do
      result <- runFile file
      case result of
        Left message -> putStrLn message >> exitFailure
        Right runResult -> putStrLn (renderRunResult runResult)
    _ -> putStrLn usage >> exitFailure

usage :: String
usage =
  unlines
    [ "usage: wyah-compiler [FILE]"
    , ""
    , "With no FILE, start the interactive ProtoHaskell shell."
    ]
