import Data.List
import Data.Char

-- from Solution 3 >>
divisible a b = a `mod` b == 0
primes = 2:[ m | m <- [3..], all (not . divisible m) (takeWhile (<= m `div` 2) primes)  ]

factors m
  | m == 1    = []
  | otherwise = p:(factors $ m `div` p)
  where p = head $ dropWhile (not . divisible m) primes
-- <<

isPrime n = n `elem` (takeWhile (<= n) primes)

toInt :: [Int] -> Int
toInt = foldl1 (\x y -> x*10 + y)


comb :: Int -> [a] -> [[a]]
comb _ []     = []
comb 0 _      = [ [] ]
comb n (x:xs) = map (x:) (comb (n-1) xs) ++ comb n xs

main = putStrLn . show . nub . map sort $ [ y |
                num  <- [1000..9999], isPrime num,
                m    <- comb 3 . nub . permutations . map digitToInt . show $ num,
                cand <- permutations m,
                let y = map toInt cand, all (> 1000) y, all isPrime y,
                (1 ==) . length . nub . zipWith (-) y $ drop 1 y
            ]


