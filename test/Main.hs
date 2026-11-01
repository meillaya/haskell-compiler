module Main (main) where

import Control.Monad (forM)
import System.Exit (exitFailure, exitSuccess)
import WYAH.TestCases

main :: IO ()
main = do
  groups <- groupedCases
  results <- forM groups $ \group_ -> do
    putStrLn ("== " <> groupName group_ <> " ==")
    outcomes <- forM (cases group_) runCase
    pure (groupName group_, outcomes)
  let allOutcomes = concatMap snd results
      passed = length [() | (True, _) <- allOutcomes]
      failed = length [details | (False, details) <- allOutcomes]
  putStrLn ""
  putStrLn
    ( "summary: "
        <> show passed
        <> " passed, "
        <> show failed
        <> " failed"
    )
  if failed == 0
    then putStrLn "All tests passed." >> exitSuccess
    else exitFailure

runCase :: Case -> IO (Bool, String)
runCase testCase = do
  result <- caseRun testCase
  case result of
    Left details -> do
      putStrLn ("[fail] " <> caseLabel testCase)
      putStrLn ("       " <> details)
      pure (False, caseLabel testCase <> ": " <> details)
    Right () -> do
      putStrLn ("[ok] " <> caseLabel testCase)
      pure (True, caseLabel testCase)
