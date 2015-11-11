import Data.List

getPolygonal 3 n = n * (n+1) `div` 2
getPolygonal 4 n = n ^ 2
getPolygonal 5 n = n * (3*n -1) `div` 2
getPolygonal 6 n = n * (2*n -1)
getPolygonal 7 n = n * (5*n -3) `div` 2
getPolygonal 8 n = n * (3*n -2)
getPolygonal x _ = 1

getDigits = map (map (`divMod` 100) . digits) [3..8]
  where digits   = takeWhile (< 10000) . dropWhile (< 1000) . list
        list   x = map (getPolygonal x) [1..]

-- getCyclic = filter (not . null) . map (filter isCyclic . permutations) . comb 
--  where isCyclic ls = and $ zipWith (==) (map snd ls) (map fst $ ls ++ [head ls])

checkCyclic :: [[(Int,Int)]] -> [[[(Int,Int)]]]
checkCyclic (x:xs) = nub $ map (\p -> checkCyclic' [p] [] xs) x
  where exploreReverse (l1:[])    rs = rs
        exploreReverse (l1:l2:ls) rs = exploreReverse (l2:ls) $ ((filter ((`elem` (map fst l1)) . snd ) l2):rs)
        checkCyclic' :: [(Int,Int)] -> [[(Int,Int)]] -> [[(Int,Int)]] -> [[(Int,Int)]]
        checkCyclic' h ls [] | not (null fin) = exploreReverse (fin:ls) [fin]
                             | otherwise      = []
          where fin = filter ((`elem` (map fst (last ls))) . snd) h 
        checkCyclic' h ls (p:ps) = checkCyclic' (filter ((`elem` (map snd h)) . fst) p) (h:ls) ps
  
-- main = putStrLn . show . getCyclic . sanitize $ getDigits
-- main = putStrLn . show . product . map length . sanitize $ getDigits
main = putStrLn . show . sum . map fromTuple . cleanList . map checkCyclic . permutations . sanitize $ getDigits

fromTuple (x,y) = x * 100 + y

cleanList = concat . nub . map sort . values . concat . map values . map (map (concat . values))
  where values = filter (not.null)


comb []           = [[]]
comb ([]:lss)     = [] 
comb ((l:ls):lss) = (map (l:) (comb lss)) ++ comb (ls:lss)


sanitize ls = if ls /= sanitized then sanitize sanitized else sanitized
  where sanitized = map (filter fl) ls
        hs = map fst $ concat ls
        ts = map snd $ concat ls
        fl n = ( (fst n) `elem` ts || (snd n) `elem` hs )
  
