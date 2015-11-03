

-- from Probrem 3 (revised) >>
divisible a b = a `mod` b == 0
primes = 2:[ m | m <- [3,5..], all (not . divisible m) (takeWhile ((<=m).(^2)) primes)  ]
-- <<

isntPrime m = m `notElem` (takeWhile (<=m) primes)
main = putStrLn . show $  take 2 list
  where
    list = [ n | n <- [ 1,3.. ], all isntPrime $ cands n ]
    cands x = [ (x - 2*(y^2)) | y <- [0..ulim x] ]
    ulim  x = floor . sqrt . fromIntegral $ (x`div`2)

