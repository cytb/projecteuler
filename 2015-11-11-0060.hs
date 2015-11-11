import Data.List
import Data.Array.IO
import Data.Ord
import Data.IORef(newIORef,modifyIORef,readIORef,writeIORef,IORef)
import Control.Monad
import GHC.Int(Int64)

import qualified Data.Vector.Unboxed.Mutable as M

import Debug.Trace

main = do
  minValRef <- newIORef 0
  -- (Primes ps pp i last) <- getPrimes 100000
  p <- getPrimes 100000000
  let (Primes ps _ i last) = p
  let ubound = 10000 :: Int
  let num    = 5 :: Int

  primes <- readListWhile p 1 (< fromIntegral ubound)
  let l = genericLength primes

  rls <- newIORef ([] :: [[Int64]])

  forM_ [0..(l-1)] $ \m -> do
    let (p1:p1s) = drop m primes
    paired <- filtPairsB p p1 p1s []
    solve p p1 paired num rls

  ls <- readIORef rls
  forM_ (sortBy (comparing sum) ls) $ \found -> do
    putStrLn $ (show found) ++ ": " ++ show (sum found)

filtPairsB _     _ []     ls = return $ reverse ls
filtPairsB prime n (x:xs) ls = do
  found <- isPair prime n x
  filtPairsB prime n xs (if not found then ls else (x:ls))

checkPairs _     []         = return True
checkPairs prime ((a,b):xs) = do
  found <- isPair prime a b
  if not found then return False else checkPairs prime xs

asTuple (x:y:[]) = (x,y)

combs 0 _      = [ [] ]
combs _ []     = []
combs n (x:xs) = map (x:) (combs (n-1) xs) ++ combs n xs

solve :: Primes Int64 -> Int64 -> [Int64] -> Int -> IORef [[Int64]] -> IO ()
solve prime n ls num rls = do
  when (genericLength ls >= (num-1)) . forM_ (combs (num-1) ls) $ \cpairs -> do
    ok <- checkPairs prime $ (map asTuple . combs 2 $ cpairs)
    when ok $ modifyIORef rls ((n:cpairs):)

isPair p a b = do
  let figs = (+1) . floor . logBase 10 . fromIntegral
  let concInt64 x y = x * (10^(figs y)) + y
  p1 <- isPrime p (concInt64 a b)
  p2 <- isPrime p (concInt64 b a)
  return (p1 && p2)

findPair :: M.IOVector (Int64,Int64) -> Int -> Int -> (Int64,Int64) -> IO Bool
findPair ps i n (a,b)
  | a >  b = findPair ps i n (b,a)
  | i >= n = return False
  | otherwise = do
    (va,vb) <- M.read ps i
    case () of
      _ | (va,vb) == (a,b) -> return True
        | va > a           -> return False
        | otherwise        -> findPair ps (i+1) n (a,b)

popPairs ps it = do
  i <- readIORef it
  val <- M.read ps i
  popPairs' val [val] 
    where
      popPairs' (pa,pb) ls = do
        i2 <- readIORef it
        writeIORef it (i2+1)
        (a,b) <- M.read ps (i2+1)
        if a /= pa then return (reverse ls)
        else popPairs' (a,b) ((a,b):ls)
  

appendPair p ar it a b = do
  found <- isPair p a b
  when found $ do
    modifyIORef it (+1)
    i <- readIORef it
    M.write ar i (a,b)

-- primes
data Primes i = Primes (IOUArray i i) (IOUArray i Bool) i i

isPrime :: Primes Int64 -> Int64 -> IO Bool
isPrime (Primes _ pp _ _) n =
  if (n `mod` 2) == 0 then return False else readArray pp (n `div` 2)

readListWhile (Primes ps _ i last) x f = listWhile x []
  where
    listWhile :: Int64 -> [Int64] -> IO [Int64]
    listWhile m ls = do
        val  <- readArray ps m
        if (m > i) || not (f val) then return (reverse ls)
        else listWhile (m+1) (val:ls)

getPrimes n = do
  let lastIndex = (n`div`2) + 1
  primes       <- newArray (1, (n`div`2) + 1) True :: (IO (IOUArray Int64 Bool))

  -- from 3
  forM_ ([3,5..n]) $ \m -> do
    cu <- readArray primes (m `div` 2)
    when (cu) . forM_ ([m*m,m*(m+2) .. n]) $ \i -> do
      writeArray primes (i `div` 2) False
  
  primeResults <- newArray (1,numPrimesM n) 0
  writeArray primeResults 1 2
  it <- newIORef 2

  forM_ [1..lastIndex] $ \m -> do
    flag <- readArray primes m
    when flag $ do
      i <- readIORef it
      writeArray primeResults i ((m*2)+1)
      modifyIORef it (+1)

  lastIndex <- readIORef it
  lastPrime <- readArray primeResults (lastIndex-1)
  return (Primes primeResults primes (lastIndex-1) lastPrime)

  where
    numPrimesM x = numPrimesM' 0 x 
      where numPrimesM' r n 
              | n <= 10    = 10
              | n <= 100   = 30 
              | n <= 1000  = 200
              | n <= (10^r) = n `div` (2*(r-1))
              | otherwise  = numPrimesM' (r+1) n

