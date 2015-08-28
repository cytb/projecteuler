sol =  [ a*b*c | a <- [1..333], b <- [a..666], c <- [b..1000], a^2 + b^2 == c^2, a + b + c == 1000 ]

main = putStrLn . show $ head sol
