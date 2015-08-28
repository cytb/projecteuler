n = 4000000

fibB = 1:1:(zipWith (+) fibB $ tail fibB)
fib = tail fibB

main = putStrLn . show . sum . takeWhile (< n) $ filter even fib
