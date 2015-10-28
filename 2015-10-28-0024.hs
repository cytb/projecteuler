main = putStrLn . concat . map show $ permutationOf [0..9] $ 1000*1000

-- repetition is not expected
permutationOf [] _ = []
permutationOf xs  1 = xs
permutationOf xs  y
  | y < 0     = permutationOf (reverse xs) y
  | otherwise = permutationOf' 1 1
  where 
    permutationOf' z zf
      | z == length xs && y <= zf = selectNums xs (y-1) (z-1) (zf `div` z)
      | z == length xs && y >  zf = permutationOf  xs (y `mod` zf)
      | otherwise                 = permutationOf' (z+1) (zf*(z+1))

    selectNums []  _ _ _ = []
    selectNums xxs _ 0 _ = xxs
    selectNums xxs n z zf = (selected:rest)
      where
        ii = (n `div` zf)
        selected = head (drop ii xxs)
        rest     = selectNums next (n `mod` zf) (z-1) (zf `div` z)
        next     = take ii xxs ++ drop (ii+1) xxs
