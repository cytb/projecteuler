
import Data.List
import Data.Array.IO
import Control.Monad

toStamp n = toStamp' n 0
  where toStamp' 0 l = l
        toStamp' n l = toStamp' d (l + 10^m)
          where (d,m) = n `divMod` 10

main = do
  let n = 10000000
  stamps <- newArray (1,n) 0 :: IO (IOUArray Int Int)

  forM_ [1..(n-1)] $ \x -> do
    writeArray stamps x (toStamp x)

  forM_ [1..((n-1) `div` 6)] $ \x -> do
    mult <- checkMult stamps x 6
    when (mult) $ do
      putStrLn . show $ x

checkMult stamps n last = do
  stamp <- readArray stamps n
  check stamp 2
  where
    check st m
      | m > last = return True
      | otherwise = do
        st2 <- readArray stamps (n*m)
        if (st2 == st) then check st (m+1) else return False
    
