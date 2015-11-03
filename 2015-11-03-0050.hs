
import Data.List
import Data.Ord

-- from Solution 3 >>
divisible a b = a `mod` b == 0
primes = 2:[ m | m <- [3..], all (not . divisible m) (takeWhile (<= m `div` 2) primes)  ]

factors m
  | m == 1    = []
  | otherwise = p:(factors $ m `div` p)
  where p = head $ dropWhile (not . divisible m) primes
-- <<

primes1m = takeWhile (<= 1000000) primes

isPrime n = n `elem` (takeWhile (<= n) primes1m)

main = putStrLn . show . head $ mostCons
  where
    mostCons    = maximumBy (comparing length) $ map consecutive primeList
    consecutive = dropWhile (not . isPrime) . reverse . scanl (+) 0 
    primeList   = zipWith drop [0..] $ repeat primes1m 

