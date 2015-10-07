import Data.List

subList (x:xs) = [[x]] ++ map (x:) (subList xs) ++ subList xs
subList []     = []

divisible a b = a `mod` b == 0
primes = 2:[ m | m <- [3,5..], all (not . divisible m) (takeWhile ((<= m).(^2)) primes)  ]

divisors 1 = [1]
divisors n = divisors' (n,[]) primes

divisors' (n,p) (m:ms)
  | divisible n  m  = divisors' ((n `div` m),(m:p)) (m:ms)
  | n >= m          = divisors' (n,p) ms
  | otherwise       = (1:reverse p)
  
d x = sum . tail . nub . (x:) .  map product . subList . divisors $ x
amicable 1 = False
amicable x = x == (d (d x)) && x /= d x

main = putStrLn . show . sum $ filter amicable [1..10000]

