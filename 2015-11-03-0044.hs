

{--
3n^2 -n - 2penta = 0
n^2 - 1/3n - 2/3penta = 0
1/6 * (1 + (sqrt(1 + 24penta))
--}

pentagonal :: Int -> Int
pentagonal n = (n*((3*n) - 1)) `div` 2

isPentaNum penta = n > 0 && pentagonal n == penta
  where n = pentaNum penta

pentaNum penta = (1+b) `div` 6
  where b = floor . sqrt $ fromIntegral (1 + 24*penta) 


    isPentaNum (m*(6*n + 3*m -1) `div` 2),
    isPentaNum ((6*n*n + 6*m*n - 2*n + 3*m*m -m) `div` 2),

-- toooooo dirty
main = putStrLn . show $ [ (pmn, pn, pmn - pn) |
    m <- [1..2000],
    n <- [1..2000],
    isPentaNum (m*(6*n + 3*m -1) `div` 2),
    isPentaNum ((6*n*n + 6*m*n - 2*n + 3*m*m -m) `div` 2),
    let pmn = pentagonal (m+n),
    let pn = pentagonal n
  ]
