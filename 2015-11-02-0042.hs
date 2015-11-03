import Data.Char
import Data.List

-- from Solution 22 >>
convert ss = convert' "" ss
convert' n (',':ss) = (reverse n:convert' "" ss)
convert' n (s:ss)   = convert' (if isLetter s then (s:n) else n) ss
convert' n []       = [reverse n]
-- <<

main = putStrLn . show . length . filter isTriangleWord . convert =<< contents
  where contents = getContents
  -- where contents = readFile "words.txt

wordValue = sum . map toInt . map toLower
  where toInt n = case elemIndex n ['a'..'z'] of 
                    Nothing -> 0
                    Just n  -> (n+1)

isTriangleWord w = (== wv) . head $ dropWhile (< wv) triangles
  where wv = wordValue w
        triangles = scanl (+) 1 [2..]



