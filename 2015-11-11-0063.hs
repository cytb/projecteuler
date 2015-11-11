


-- by math, 1 <= x < 10
--  and 0 < n , n <= 1/(1-log x)
--  n `th` power: n >= 4 ?

getUbN x = ceiling (1 / (1 - logBase 10 (fromIntegral x))) + 1
fig = (+1) . floor . logBase 10 . fromIntegral

main = putStrLn $ (show $ length list) ++ ": " ++ show list
  where list = [ (x,n,fig (x^n)) | x <- [1..9], n <- [1 .. getUbN x], fig (x^n) == n ]
