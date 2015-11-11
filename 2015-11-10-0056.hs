import Data.List

toReversedList 0 = []
toReversedList n = (m:toReversedList d)
  where (d,m) = divMod n 10

main = putStrLn . show . maximum $ [ sum . toReversedList $ (a^b) | a <- [1..100], b <- [1..100] ]

