import Data.List(nub)

main = putStrLn . show . length . nub $ [ a^b | a <- [2..100], b <- [2..100] ]
