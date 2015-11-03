import Data.List
import Data.Char

listToInt = foldl (\x y-> x * 10 + y) 0

main = putStrLn . show . sum . map listToInt $ connect divList last2
  where
    divList = reverse [1,2,3,5,7,11,13,17]
    last2   = [ (x:y:[]) | x <- [0..9], y <- [0..9], x /= y ]
    connect    []     rss = rss
    connect    (d:ds) rss = connect ds $ concatMap (connectOne d) rss
    connectOne d rs = filter noDup . map (:rs) $ connDiv d rs
    noDup      x    = length x == (length $ nub x)
    connDiv    d rs = [ x `div` 100 | x <- divs d, connective x rs ]
    connective x rs = (x `mod` 100) == (listToInt (take 2 rs))
    divs       d    = [ x | x <- [1..999], x `mod` d == 0 ]

