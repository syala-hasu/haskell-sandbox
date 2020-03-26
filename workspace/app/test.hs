out a b c s = putStrLn $ unwords [show (a + b + c), s]
readInput = do
  a <- readLn
  [b,c] <- map read . words <$> getLine
  s <- getLine
  out a b c s

main = readInput
