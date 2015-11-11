import Control.Monad
import Data.Array.IO
import Data.Word
import Data.IORef

data Primes i = Primes (IOUArray i i) i i

progressPrime primes last i n nr = do
  d <- readArray primes i
  case () of 
    _ | last <= i || nr < d -> writeArray primes (last+1) n >> return (last+1)
      | n `mod` d /= 0      -> progressPrime primes last (i+1) n nr
      | otherwise           -> return last

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

isPrime (Primes ps last p) n
  | n <  2    = return False
  | p == n    = return True
  | p <= 2    = return True
  | otherwise = findNext ps (1,2) (last,p) n

prepareSum buffer sub (Primes primes _ _) s i = do
  val <- readArray primes i
  writeArray buffer (i-sub) (s + val)
  return (s + val)

finder primes buffer i (ni,nv)
  | i < ni    = return (ni,nv)
  | otherwise = do
    val <- readArray buffer i
    p   <- isPrime primes val
    if p then return (i,val)
    else finder primes buffer (i-1) (ni,nv)

main = do
  primes <- getPrimes (10000000 :: Int)
  let (Primes ps i _) = primes
  buffer <- ( newArray (1,i) 0 :: IO (IOUArray Int Int))
  conc   <- newIORef (1,2)

  forM_ [0..(i-1)] $ \a -> do
    m <- readIORef conc
    if a `mod` 1000 == 0 then
      putStrLn ("drop: "++ show a ++ " -> " ++ show m)
      else return ()
    if fst m < (i-a)
    then foldM (prepareSum buffer a primes) 0 [(a+1)..i]
    else return 0
    n <- finder primes buffer (i-a) m
    writeIORef conc n
    return ()

  readIORef conc >>= putStrLn . ("(index,value) = "++) . show


