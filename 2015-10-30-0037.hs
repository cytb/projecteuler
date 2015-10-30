
import Data.List 

main = putStrLn . show . sum $ take 11 subpPrime

-- from Probrem 3 (revised) >>
divisible a b = a `mod` b == 0
primes = 2:[ m | m <- [3,5..], all (not . divisible m) (takeWhile ((<=m).(^2)) primes)  ]
-- <<

isPrime n = n == (head $ dropWhile (< n) primes)

subpPrime = filter find . filter (> 9) $ primes
  where find = all isPrime . map read . filter (not . null) . revSequences . show
        revSequences xs = subs xs ++ (map reverse $ subs (reverse xs))
          where subs ns = zipWith drop [0..(length ns)-1] $ repeat ns

