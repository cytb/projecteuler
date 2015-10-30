import Data.List

main = putStrLn . show . sum . figComb $ [1..9]

numComb fig xxs = map snd . filter fst . map (numComb' fig) $ permutations xxs
  where
    numComb' (a,b) xs = (l*m==n,n)
      where
        l = listToInt 0 (take a xs)
        m = listToInt 0 . take b $ drop a xs
        n = listToInt 0 $ drop (a+b) xs
        listToInt y []     = y
        listToInt y (x:xs) = listToInt (y*10 + x) xs 

figComb xs = nub . concat . map (\x -> x xs) $ map numComb figures
  where w       = length xs
        figures = [ (a,b) | a <- [1..(w-2)],
                            b <- [1..(w-a-1)],
                            a <= b,
                            2*(a+b) == w || 2*(a+b)==w+1 ]

