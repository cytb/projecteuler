import Data.Char

main = putStrLn . show . product $ [ decN 0 (10^n) | n <- [0..6] ]
  where decN x n | n <= charCount = (!! m) . map digitToInt . show $ (d + 10^x)
                 | otherwise           = decN (x+1) (n - charCount)
                   where (d,m) = (n-1) `divMod` (x+1)
                         charCount = 9*(10^x)*(x+1) 
        ss = take 10000 . concat $ map show [1..]

