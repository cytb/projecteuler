
import Data.Ord
import Data.Function
import Data.List

import GHC.Int(Int64)

toList i = toList' i []
  where toList' 0 rs = rs
        toList' x rs = toList' d (m:rs)
          where (d,m) = x `divMod` 10

fromList = foldl progress 0
  where progress n x = n*10 + x

main = putStrLn . show . head . cleanList . head $ find (10000 :: Int64)
  where
    trd  (_,_,t) = t
    ssnd (_,t,_) = t
    cleanList = sort . (map ssnd)
    find  = filter ((5 <=) . length). groupCube . map toTup . enumFromTo 1
    groupCube = groupBy ((==) `on` trd) . sortBy (compare `on` trd) 
    toTup i = (i,cube,fromList . (genericLength list:) $ list)
      where list = sort . toList $ cube
            cube = i^3

  




