fibonacciFind f
  | f 1 = (1,1)
  | otherwise = findNext 1 1 3
  where
    findNext x y z | f (x+y) = (z,(x+y))
                   | otherwise = findNext y (x+y) (z+1)

main = putStrLn . show $ fibonacciFind (>= 10^(1000-1))
