import Control.Monad
import Data.Array.IO
import Data.Word
import Data.IORef
import Data.List
import Data.Char
import GHC.Int(Int32)

toList :: Int -> [Int]
toList = map digitToInt . show

toInt :: [Int] -> Int
toInt  = foldl (\n m -> n*10 + m) 0

tMap f (a,b) = (f a, f b)
masked a bits = tMap (fst . unzip) . partition snd $ zip a bits

bitPatterns x = nub . concatMap permutations $ zipWith bitComb (replicate (x-1) x) [1..]
  where bitComb n s = take (n-s) (repeat True) ++ take s (repeat False)

modifyArray a i f = readArray a i >>= (writeArray a i . f)

generateResultSet =
  foldM next [] $ concatMap (map (++ [False]) . bitPatterns) [2..7]
    where
      next :: [(Int,[Bool], IOArray Int Int)] -> [Bool] -> IO [(Int,[Bool], IOArray Int Int)]
      next results mask = do
        let elems = 10^(length $ filter (not.id) mask)
        resultArray <- newArray (1,elems) 0
        return ((length mask,mask,resultArray):results)

process primes = do
  let (Primes ps i _) = primes

  results <- generateResultSet

  forM_ [1..i] $ \n -> do
    prime <- readArray ps n
    next results $ toList prime 

  forM_ results $ \(len,mask,rarray) -> do
    (_,elems) <- getBounds rarray 
    forM_ [1..elems] $ \n -> do
      res <- readArray rarray n
      when (res >= 8) $ do
        found <- foldM (givenPrimes mask n) [] [0..9] 
        putStrLn . show . minimum $ found

  where
    next [] prime = return ()
    next ((len,maskBits,arr):resultSet) prime = do
      when (len == length prime) $ do
        let (hidden,shown) = masked prime maskBits
        when ((length $ nub hidden) == 1) $ modifyArray arr (toInt shown) (+1)
      next resultSet prime

    givenPrimes mask shown ls n = do
      let mergedNum = toInt $ merge mask (toList shown) n
      doAdd <- isPrime primes mergedNum
      return (if doAdd then (mergedNum:ls) else ls)

    merge [] _  _ = []
    merge (m:mask) (s:shown) num
      | m         = (num:merge mask (s:shown) num)
      | otherwise = (s:merge   mask shown num)

main = do
  primes <- getPrimes (1000000 :: Int)
  process primes

-- primes
data Primes i = Primes (IOUArray i i) i i

getPrimes :: Int -> IO (Primes Int)
getPrimes n = do
  primes    <- newArray (1,n) 0
  writeArray primes 1 2
  lastIndexRef <- newIORef 1

  forM_ [3,5..n] $ \m -> do
    last <- readIORef lastIndexRef
    let m2 = (floor . sqrt . fromIntegral $ m)
    i    <- progressPrime primes last 2 m m2
    writeIORef lastIndexRef i
    return ()

  lastIndex <- readIORef lastIndexRef
  lastPrime <- readArray primes lastIndex
  return (Primes primes lastIndex lastPrime)
  where 
    progressPrime primes last i n nr = do
      d <- readArray primes i
      case () of 
        _ | last <= i || nr < d -> writeArray primes (last+1) n >> return (last+1)
          | n `mod` d /= 0      -> progressPrime primes last (i+1) n nr
          | otherwise           -> return last



isPrime :: (Primes Int) -> Int -> IO (Bool)
isPrime (Primes ps last p) n
  | n <  2    = return False
  | p == n    = return True
  | p <= 2    = return True
  | otherwise = findNext ps (1,2) (last,p) n
  where 
    findNext ps (boti,botv) (topi,topv) n
      | topi - boti < 2 = return False
      | otherwise = do
        let fi = fromIntegral
        let im = floor (fi n * ((fi boti / fi botv) + (fi topi / fi topv) ) / 2)
        let i  = if im <= boti || topi <= im then (boti + topi) `div` 2 else im
        p <- readArray ps i
        case () of
          _ | p == n    -> return True
            | p <  n    -> findNext ps (i,p) (topi,topv) n
            | otherwise -> findNext ps (boti,botv) (i,p) n

