import Data.List
import Data.Function

floatCycle :: Integer -> [Integer]
floatCycle n = floatCycle' 1 []
  where
    floatCycle' d xs
      | rest == 0 = []
      | rest `elem` xs = (rest:takeWhile (/= rest) xs)
      | otherwise = floatCycle' (rest*10) (rest:xs)
      where
        rest = d `mod` n
        
        
main = putStrLn . show $ maximumBy (compare `on` snd) lenList
  where
    lenList = zipWith (,) [1..] . map (length . floatCycle) $ [1..n]


