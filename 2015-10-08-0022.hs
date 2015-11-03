import Data.Char
import Data.List


toNum ss = sum . map (numc . toUpper) $ ss
numc  :: Char -> Integer
numc c   = (1 +) . fromIntegral . length $ takeWhile (/= c) "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

convert ss = convert' "" ss
convert' n (',':ss) = (reverse n:convert' "" ss)
convert' n (s:ss)   = convert' (if isLetter s then (s:n) else n) ss
convert' n []       = [reverse n]

solve = sum . zipWith (*) [1,2..] . map toNum . sort . convert 

main = putStrLn . show . solve =<< getContents
  
  
