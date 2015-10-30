
main = putStrLn . show . sum . filter isBinaryPalind $ palind10lt 1000000

palind10lt n = map read . concat $ take (length . show $ (n-1)) inifiniteL
  where
    inifiniteL = (numChars:palind10' 1)
    numChars   = map show [0..9]
    palind10' n = (lsEven:lsOdd:palind10' (n+1))
      where
        lsEven    = [ nS ++ reverse nS       | nS <- numStr ]
        lsOdd     = [ nS ++ nC ++ reverse nS | nS <- numStr, nC <- numChars ]
        numStr = map show [10^(n-1) .. (10^n)-1]

isBinaryPalind = binary' []
  where
  	binary' m 0 = reverse m == m
  	binary' m n = binary' ((n `mod` 2):m) (n `div` 2)

