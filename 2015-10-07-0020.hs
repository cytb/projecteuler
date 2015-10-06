import System.IO

remove0 r = if r < 10 || r `mod` 10 /= 0 then r else remove0 (r `div` 10)

factSum 0 = 1
factSum x = remove0 x * remove0 (factSum (x-1))

sumDigits x
 | x < 10    = x
 | otherwise = x `mod` 10 + sumDigits (x `div` 10)

main = putStrLn . show . sumDigits . factSum $ 100
