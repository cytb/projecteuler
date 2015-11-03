
import Data.List

main = putStrLn . show .  maximum . concat . map primePandigitals $ [1..9]

toInt = foldl (\a b -> a*10 + b) 0

primePandigitals n = filter isPrime . map toInt . permutations $ [1..n]

-- from Probrem 3 (revised) >>
divisible a b = a `mod` b == 0
primes = 2:[ m | m <- [3,5..], isPrime m ] 
isPrime x = all (not . divisible x) (takeWhile ((<=x).(^2)) primes)
-- <<

