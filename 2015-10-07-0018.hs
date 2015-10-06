import System.IO

toIntList = map (map (read :: String -> Integer) . words) . lines

maxLs prev next = zipWith max l r
  where
    l = zipWith (+) next prev
    r = zipWith (+) next (drop 1 prev)

main = do
    arr <- getContents >>= return . toIntList
    putStrLn . show . foldl1 maxLs . reverse $ arr

