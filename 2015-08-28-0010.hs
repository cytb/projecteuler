
-- from Solution 3
divisible a b = a `mod` b == 0
primes = 2:[ m | m <- [3,5..], all (not . divisible m) (takeWhile ((<= m).(^2)) primes)  ]
--

main = putStrLn . show . sum $ takeWhile (< 2000000) primes


