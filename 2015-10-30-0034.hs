import Data.List

main = putStrLn . show . sum . filter (>= 3) $ curious

factorial 0 = 1
factorial n = n * factorial (n-1)

decompose 0 = []
decompose n = (n `mod` 10):decompose (n `div` 10)

fig 0 = 0
fig n = 1 + fig (n `div` 10)

curious = map sumFc . filter cmpSum $ combos mxFig [0..9]
 where
   cmpSum x = x == (sort . decompose $ sumFc x)
   sumFc    = sum . map factorial
   mxFig    = last $ takeWhile ltF9 [1..]
   ltF9 x    = x <= fig (x *  factorial 9)

combos m ls = concatMap ((flip combo) ls) $ [1..m]
  where combo _ []     = []
        combo 0 _      = [[]]
        combo n (x:xs) = map (x:) (combo (n-1) (x:xs)) ++ combo n xs
  

