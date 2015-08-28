import Data.List(tails,transpose)

startswith a b = take (length a) b == a
split jj = reverse . map reverse . splitr [] "" jj
  where
    splitr l s j xs@(x:_) = if startswith j xs then splitr (s:l) "" j (drop (length j) xs)
                            else (splitr l (x:s) j $ tail xs)
    splitr _ _ "" xs = map (:[]) xs
    splitr l [] _  [] = l
    splitr l s  _  [] = (s:l)

readContent = do
  line <- getLine
  if null line then return [] else readContent >>= return . (line:)

toList = map (map (read :: String -> Integer) . split " ")

lists n l =  concat . concat $ ((calc n $ transpose l):(calc n l):(diagl n l):(diagl n $ map reverse l):[])
  where
    calc n     = map ( map (product . take n) . tails)
    diagl n ll = (first:rest)
      where first = ( map (product . map head . zipWith drop [0..n-1]) $ tails ll)
            rest  = if all ((> n ) . length) ll then (diagl n (map tail ll)) else []

main = do
  putStrLn . show . maximum . lists 4 . toList =<< readContent
  