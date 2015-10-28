import Data.Function
import Data.List

-- from Solution 3 >>
divisible a b = a `mod` b == 0
primes = 2:[ m | m <- [3..], all (not . divisible m) (takeWhile (<= m `div` 2) primes)  ]

factors m
  | m == 1    = []
  | otherwise = p:(factors $ m `div` p)
  where p = head $ dropWhile (not . divisible m) primes
-- <<

calcFormula :: (Int,Int) -> Int -> Int
calcFormula (a,b) n = (n^2) + a*n + b

isPrime n = n == (head $ dropWhile (< n) primes)
comb n = [ (a,b) | b <- takeWhile (< n) primes,
                   a <- [-n,-n+1..n-1], 
                   b > -a ]

quadratic c = (c, length . takeWhile (isPrime . calcFormula c) $ [0..])

productTup (a,b) = (a*b)

main = putStrLn . show . productTup . fst $ found
  where
    found = maximumBy (compare `on` snd) . map quadratic $ comb 1000


