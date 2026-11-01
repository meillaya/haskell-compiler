greet name = do
  putStr "Hello, "
  putStrLn name

main = do
  putStrLn "What is your name?"
  name <- getLine
  greet name
