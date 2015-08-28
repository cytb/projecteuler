import Data.List(nub,group,sort)
 
tries = (1:3:(zipWith (+) [3..] (tail tries))) :: [Integer]

ms = (2:3:([ (m+k) | m <- [6,12..], k <- [-1,1]] :: [Integer]))
factors n = factorsr n ms
  where
    factorsr 1  _        = []
    factorsr nn (mm:mms) = if mm * mm > nn then [nn] else if nn `mod` mm == 0 then (mm:(factorsr (nn `div` mm) (mm:mms))) else factorsr nn mms

main = putStrLn . show . head . dropWhile ((< 500) . product . map ((+ 1) . (length)) . group . sort . factors) $ tries

