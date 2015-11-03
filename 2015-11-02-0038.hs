
import Data.List

main = putStrLn . concat . map show $ concProd
  where concProd = take 9 . concat $  map (multList mx) [1..]
        mx       = maximum . concat $ enumerate

check m ls = (checkUnique m calcls) && (checkLen m calcls)
  where calcls = map (multList ls) [1..]
        checkUnique n = (n ==) . length . filter (/= 0) . nub . take n . concat
        checkLen n (ll:lls) | n > 0     = checkLen (n- length ll) lls
                            | otherwise = n == 0

multList ls n = dropWhile (== 0) . uncurry (:) $ mapAccumR (accumCalc n) 0 ls
  where accumCalc n x y = (n*y + x) `divMod` 10

enumerate = map (filter (check 9) . srcs) [1..4]
  where srcs  n  = filter ((/= 5) . last) $ permute n [1..9]
        
permute k = concat . map permutations . combination k
combination 0 _      = [[]]
combination n []     = []
combination n (x:xs) = map (x:) (combination (n-1) xs) ++ combination n xs
