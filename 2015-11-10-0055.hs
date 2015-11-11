import Data.List
import Data.Char

isPalindrome n = toList n == toReversedList n

toList = reverse . toReversedList
toReversedList 0 = []
toReversedList n = (m:toReversedList d)
  where (d,m) = divMod n 10

palindR x = x + (foldl (\n m -> n*10 + m) 0 $ toReversedList x)

isLychrelNumber 0  n = True
isLychrelNumber lt n
  | isPalindrome palind = False
  | otherwise           = isLychrelNumber (lt-1) palind
  where palind = palindR n

main = putStrLn . show . length  $ filter (isLychrelNumber (50-1)) [ 1.. (10000-1) ]

