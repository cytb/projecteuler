import System.Random

main = newStdGen >>= putStrLn . show . calc 1200000 . randoms 

calc :: Int -> [Double] -> Double
calc n (r:rs) = 4 * fromIntegral m / fromIntegral n
  where
    m = snd . foldr collect (r,0) $ take (n+1) rs
    collect y (x,t) = (y, t + fromEnum (sqrt (x*x + y*y) < 1))
