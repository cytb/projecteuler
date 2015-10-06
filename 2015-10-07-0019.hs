import System.IO

data WeekDay = Sun | Mon | Tue | Wed | Thu | Fri | Sat
  deriving (Show, Eq)

weekdays = [ Mon, Tue, Wed, Thu, Fri, Sat, Sun ]

divisible x y = x `mod` y == 0

feb :: Int -> Int
feb year
  | year `divisible` 400 = 29
  | year `divisible` 100 = 28
  | year `divisible` 4   = 29
  | otherwise            = 28

daysOfMonth :: Int -> Int -> Int
daysOfMonth year 2 = feb year
daysOfMonth year m = if any (m ==) [4,6,9,11] then 30 else 31

daysOfYear year  = feb year + (365 - 28)

firstSundaysIn :: Int -> Int
firstSundaysIn year = length . filter ((==Sun) . weekDayFrom1900Of) $ days
 where
  weekDayFrom1900Of x = head $ drop (x `mod` length weekdays) weekdays
  daysFrom1900Before  = (sum $ map daysOfYear [1900 .. year]) - daysOfYear year
  days = map (daysFrom1900Before +) $ sumfold (map (daysOfMonth year) [1..11])

sumfold = reverse . foldl (\x y-> (head x + y):x) [0]

main = putStrLn . show . sum . map firstSundaysIn $ [1901 .. 2000]
