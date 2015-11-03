import Data.List
import Data.Ord

main = putStrLn . show . head . maximumBy (comparing length) . group $ sort [ a + b + c |
    a <- [1..(1000 `div` 3)], b <- [a..1000-(2*a)],
    let c = floor . sqrt . fromIntegral $ (a^2 + b^2),
    a^2 + b^2 == c^2
    ]

