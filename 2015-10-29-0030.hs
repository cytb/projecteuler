import Data.List(nub)

main = putStrLn . show .  solveNth $ 5

solveNth n = sum . nub . filter (> 1) . concat . map enumNums $ repComb (n+1) [0..9]
  where
    listToNum = foldl (\t x-> t * 10 + x) 0
    enumNums ls = filter (== sum (map (^n) ls)) . map listToNum $ perm ls

repComb 0 _   = []
repComb _ []  = []
repComb 1 xs  = map (:[]) xs 
repComb n (x:xs) = map (x:) (repComb (n-1) (x:xs)) ++ repComb (n) (xs)

perm []       = [[]]
perm (xx:xxs) = concat . map nextPerm $ swapped
  where 
    nextPerm  []     = []
    nextPerm  (x:xs) = map (x:) $ perm xs
    swapped = ((xx:xxs):(perms' [] (xx:xxs)))
    perms' [] []         = []
    perms' (x:xs) []     = []
    perms' [] (y:ys)     = perms' (y:[]) ys
    perms' (x:xs) (y:ys) = (((y:xs) ++ (x:ys)): perms' ((y:xs)++[x]) ys)
