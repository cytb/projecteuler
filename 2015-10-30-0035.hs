
import Data.List
import Data.Word(Word32)

-- from Probrem 3 (revised) >>
divisible a b = a `mod` b == 0
primes = 2:[ m | m <- [3,5..], all (not . divisible m) (takeWhile ((<=m).(^2)) primes)  ]
-- <<

main = putStrLn . show . lenCon . sort . nub $ searchCircularPrime 1000000 (primes :: [Word32]) []
lenCon x = (length x, x)

searchCircularPrime n []     rs = rs
searchCircularPrime n (p:ps) rs
  | p < 10      = searchCircularPrime n ps (rs ++ cycleLists)
  | p > n       = rs
  | p `elem` rs = searchCircularPrime n ps rs
  | found       = searchCircularPrime n ps (rs ++ cycleLists)
  | otherwise   = searchCircularPrime n ps rs
  where num m []     = m
        num m (x:xs) = num (m*10 + x) xs
        found      = all search cycleLists
        search m   = m == (head . dropWhile (<m) $ (p:ps))
        cutter     = map (num 0 . take l) . zipWith drop [0..(l-1)]
        cycleLists = cutter . repeat . cycle $ decompose p
        l          = length $ decompose p

decompose = decompose' []
  where decompose' m 0 = m
        decompose' m n = decompose' ((n `mod` 10):m) (n `div` 10)
