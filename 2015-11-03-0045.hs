
triangle   n = n*(  n + 1) `div` 2
pentagonal n = n*(3*n - 1) `div` 2
hexagonal  n = n*(2*n - 1)

isTriangle x = triangle m == x
  where m = (-1 + (floor . sqrt . fromIntegral $ (1+8*x))) `div` 2

isPentagonal x = pentagonal m == x
  where m = (1 +  (floor . sqrt . fromIntegral $ (1+24*x))) `div` 6

isHexagonal x = hexagonal m == x
  where m = (1 +  (floor . sqrt . fromIntegral $ (1+8*x))) `div` 4


main = putStrLn . show $ take 3 [ h | h <- [ hexagonal x | x <- [1..]], isPentagonal h, isTriangle h ]

  
