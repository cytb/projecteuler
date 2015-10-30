
import Data.Function
import Data.List

main = putStrLn . show . snd . reduceT $ foldr multT (1,1) ls
  where com c n = [ c*10 + n, n*10 + c ]
        ls      = [ (nn,dd) | (n,d) <- [ (n,d) | n <- [1..9], d <- [1..9], n < d ],
                              c <- [1..9], nn <- com c n, dd <- com c d,
                              nn < dd, reduce n d == reduce nn dd ]

        multT (x,y) (m,n) = (x*m,y*n)
        reduceT (x,y) = reduce x y

figs n | n < 0  = figs (-n)
       | 10 < n = 1
       | otherwise = 1 + figs (n `div` 10)

reduce a b = reduce' 1 1 (factors a) (factors b)
  where reduce' n m []     []     = (n,m)
        reduce' n m xs     []     = ((product (n:xs)),m)
        reduce' n m []     ys     = (n,(product (m:ys)))
        reduce' n m (x:xs) (y:ys) | x < y = reduce' (x*n) m     xs     (y:ys)
                                  | x > y = reduce' n     (y*m) (x:xs) ys
                                  | otherwise = reduce' n m xs ys

lcd =  product . map calcPower . selectMaxFactors . concat .  map (map tuplify . group . factors)
  where tuplify xs       = (head xs,length xs)
        calcPower (x,n)  = x^n
        selectMaxFactors = map (maximumBy cmpM) . groupBy cmpT . sortBy cmp
        cmp              = (compare `on` fst)
        cmpT a b         = fst a == fst b
        cmpM             = (compare `on` snd)
        
-- from Solution 3 >>
divisible a b = a `mod` b == 0
primes = 2:[ m | m <- [3..], all (not . divisible m) (takeWhile (<= m `div` 2) primes)  ]

factors m
  | m == 1    = []
  | otherwise = p:(factors $ m `div` p)
  where p = head $ dropWhile (not . divisible m) primes
-- <<
