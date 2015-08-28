import Data.List(union)

a = 3
b = 5
n = 1000

main = putStrLn . show . sum $ union [a*1,a*2..n-1] [b*1,b*2..n-1]
