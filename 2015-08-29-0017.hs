
writelen :: Int -> Int
writelen n
  | 1000 <= n             = writelen (n `div` 1000) + (length "thousand")
  | 100  <= n && n < 1000 = writelen (n `div` 100 ) + (length "hundred")  + writelen (n `mod` 100) + 
                            (if (n `mod` 100) > 0 then (length "and") else 0)
  | 20   <= n && n < 100  = lend (n `div` 10)   decades + writelen (n `mod` 10)
  | otherwise             = lend n              digits
  where
    lend m    = length . head . drop m
    decades   = [[],[],"twenty","thirty","forty","fifty","sixty","seventy","eighty","ninety"]
    digits    = [
         [],"one","two","three","four","five","six","seven","eight","nine",
         "ten","eleven","twelve","thirteen","fourteen","fifteen","sixteen","seventeen","eighteen","nineteen"
      ]


main = putStrLn . show . sum . map writelen $ [1..1000] 
.
