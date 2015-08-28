
toList :: String -> [Integer]
toList = map read . words

main = do
  getContents >>= putStrLn . take 10 . show . sum . toList
