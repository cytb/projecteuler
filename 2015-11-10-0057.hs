import Data.List

denon2 (n,d) _ = ((d,nextD),(d + nextD,nextD))
  where nextD = 2*d + n

main = putStrLn . show . length . filt $ mapAccumL denon2 (1,2) [1..(1000-1)]
    where filt = filter (\(n,d) -> len n > len d) . snd
          len = length . show

