
n = 600851475143

divisible a b = a `mod` b == 0
primes = 2:[ m | m <- [3,5..], all (not . divisible m) (takeWhile ((<= m).(^2)) primes)  ]

factors m
  | m == 1    = []
  | otherwise = p:(factors $ m `div` p)
  where p = head $ dropWhile (not . divisible m) primes

main = putStrLn . show $ factors n
