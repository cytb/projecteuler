import Data.List

main = putStrLn . show . length . findWays 200 $ [1,2,5,10,20,50,100,200]

findWays value = findWays' value . reverse . sort
  where
    findWays' _    []      = []
    findWays' _    (c:[])  = [ replicate (value `div` c) c ]
    findWays' 0    _       = [[]]
    findWays' value (c:cs) | value <  0 = []
                           | otherwise  = found ++ findWays' value cs
      where found          | c > value  = []
                           | otherwise  = map (c:) (findWays' (value-c) (c:cs))
