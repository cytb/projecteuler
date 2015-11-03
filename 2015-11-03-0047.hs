
import Data.List

-- from Solution 3 >>
divisible a b = a `mod` b == 0
primes = 2:[ m | m <- [3..], all (not . divisible m) (takeWhile (<= m `div` 2) primes)  ]

factors m
  | m == 1    = []
  | otherwise = p:(factors $ m `div` p)
  where p = head $ dropWhile (not . divisible m) primes
-- <<

main = putStrLn . show . (+1) . sum . map fst $ takeWhile dropper distinctF
  where distinctF  = map lenHead . group $ factorList 
        factorList = [ length . group $ factors n | n <- [1..] ]
        lenHead n  = (length n, head n)
        dropper (len,n) = len < 4 || n /= 4

