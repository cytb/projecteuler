import Data.List
import Debug.Trace

nCrM :: Integer -> Integer -> Integer
nCrM n r
  | r >= n    = 1
  | r < 1     = 1
  | r > (n-r) = nCrM n (n-r)
  | otherwise = product (take (fromIntegral r) [n,(n-1)..]) `div` product [1..r]
  
nCrS n = map (nCrM n) [1..n]

main = putStrLn . show . length $ thanMillion
  where thanMillion = filter (> 1000000) $ concatMap nCrS [1..100]
