module Main where

import System.Exit (exitFailure)
import WYAH.TestCases

main :: IO ()
main = do
  groups <- groupedCases
  mapM_ runGroup groups
  putStrLn "All tests passed."

runGroup :: CaseGroup -> IO ()
runGroup group_ = do
  putStrLn ("== " <> groupName group_ <> " ==")
  mapM_ runCase (cases group_)

runCase :: Case -> IO ()
runCase testCase =
  case testCase of
    AssertionCase label condition -> assert label condition

assert :: String -> Bool -> IO ()
assert label condition =
  if condition
    then putStrLn ("[ok] " <> label)
    else do
      putStrLn ("[fail] " <> label)
      exitFailure
