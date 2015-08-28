import Data.List
import Data.Function

-- from Solution 3 >>
divisible a b = a `mod` b == 0
primes = 2:[ m | m <- [3..], all (not . divisible m) (takeWhile (<= m `div` 2) primes)  ]

factors m
  | m == 1    = []
  | otherwise = p:(factors $ m `div` p)
  where p = head $ dropWhile (not . divisible m) primes
-- <<


nfactors =   foldl (*) 1
           . concat . map (\x -> replicate (snd x) (fst x))
           . map (maximumBy (compare `on` snd))
           . groupBy ((==) `on` fst) . sortBy (compare `on` fst)
           . concat . map (map (\x -> (head x, length x) ) . group . factors)

main = putStrLn . show $ nfactors [1..20]
