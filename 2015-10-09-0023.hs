import Data.List
import Data.Array.IO

-- from Probrem 21
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
-- end Probrem 21

abuU = 28123

abu  = filter (\x -> x < d x) [1,2..abuU]
abu2 = abu2' abu abu abu

abu2' _  [] _ ar = return ()
abu2' [] _  _ ar = return ()
abu2' (a:as) (a2:a2s) (n:ns) ar
  | (a + a2) > abuU = abu2' as (n:ns) ns ar
  | (a + n ) > abuU = return ()
  | otherwise       = writeArray ar (a+a2) False >> abu2' (a:as) a2s (n:ns) ar

sumArray arr st mx cu
  | st <= mx = do
  	x <- readArray arr st
  	sumArray arr (st+1) mx (if x then cu + st else cu)
  | otherwise = return cu

main = do 
  arr <- newArray (0,abuU) True :: IO (IOArray Int Bool)
  abu2 arr >> sumArray arr 1 abuU 0 >>= putStrLn . show

