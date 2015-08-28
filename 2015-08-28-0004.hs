palind x = x == reverse x

ar = [100..999] :: [Int]
mn = [ m*n | m <- ar, n <- ar, palind $ show (m*n) ]

main = putStrLn . show . maximum $ mn

