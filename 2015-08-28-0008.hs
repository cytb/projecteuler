readContent = do
  line <- getLine
  if null line then return []
  else readContent >>= return . (line:)

numbers _ []     = []
numbers n (x:xs) = (take n (x:xs)): (numbers n xs)

main = do
  digits <-  (readContent >>= return . map (read :: String -> Integer) . map (:[]) . concat)
  putStrLn . show . maximum . map (foldl (*) 1) $ numbers 13 digits
.
