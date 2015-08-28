main = putStrLn $ show ((sum [1..100])*(sum [1..100])  - sum [ m*m | m <- [1..100] ])
