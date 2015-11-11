import Data.List
import Data.Array.IO
import Data.IORef
import Control.Monad
import GHC.Int

spiralDiags 1 = [1]
spiralDiags 2 = [1,2,3,4]
spiralDiags n = pre ++ tail (scanl (+) (last pre) $ replicate 4 (n-1))
  where pre = spiralDiags (n-2)

readWhileLessThan ar i n ls = do
  val <- readArray ar i
  if (val < n) then readWhileLessThan ar (i+1) n (val:ls)
  else return (i,(val:ls))

findSpiral :: IOUArray Int64 Int64 -> IORef Int64 -> Int64 -> IO ()
findSpiral ps ref 1 = findSpiral' ps 1 ref 3 1 0 1

findSpiral' :: IOUArray Int64 Int64 -> Int64 -> IORef Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> IO ()
findSpiral' ps i ref n total pms lst = do
  let nextLast = lst + ((n-1)*4)
  (nextI,pss) <- readWhileLessThan ps i nextLast []
  let nextPs = genericLength . filter (`elem` pss) $ zipWith (+) (repeat lst) (map (*(n-1)) [1,2,3,4])
  if ((total+4) < (pms + nextPs)*(10 :: Int64) )
  then do
    putStrLn $ "n: " ++ show n ++ ", total: " ++ show (total+4) ++ ", (pms + nextPs)*10: " ++ show ((pms + nextPs)*10)
    findSpiral' ps nextI ref (n+2) (total+4) (pms + nextPs) nextLast
  else writeIORef ref n
  
main = do
  minValRef <- newIORef 0
  (Primes ps i last) <- getPrimes 1000000000

  findSpiral ps minValRef 1
  minVal <- readIORef minValRef
  putStrLn . ("below 10% ... " ++) $ show minVal
  -- below 10% ... 26241

-- primes
data Primes i = Primes (IOUArray i i) i i

getPrimes :: Int64 -> IO (Primes Int64)
getPrimes n = do
  let lastIndex = (n`div`2) + 1
  primeResults <- newArray (1,numPrimesM n) 0
  primes       <- newArray (1, (n`div`2) + 1) True :: (IO (IOUArray Int64 Bool))
  writeArray primeResults 1 2

  -- from 3
  forM_ ([3,5..n]) $ \m -> do
    forM_ ([m*m,m*(m+2) .. n]) $ \i -> do
      writeArray primes (i `div` 2) False
  
  it <- newIORef 1
  forM_ [1..lastIndex] $ \m -> do
    flag <- readArray primes m
    when flag $ do
      i <- readIORef it
      writeArray primeResults i ((m*2)+1)
      modifyIORef it (+1)

  lastIndex <- readIORef it
  lastPrime <- readArray primeResults (lastIndex-1)
  return (Primes primeResults (lastIndex-1) lastPrime)

  where
    numPrimesM x = numPrimesM' 0 x 
      where numPrimesM' r n 
              | n <= 10    = 10
              | n <= 100   = 30 
              | n <= 1000  = 200
              | n <= (10^r) = n `div` (2*(r-1))
              | otherwise  = numPrimesM' (r+1) n

