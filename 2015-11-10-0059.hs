import  Data.List
import  Data.Char
import  Data.Bits
import  Data.Ord
import  System.IO


decrypt string = zipWith xorChar string . cycle
  where a `xorChar` b = chr (ord a `xor` ord b)

candidates string = filter (all isPrint . decrypt string) passwords
  where passwords = [ (a:b:c:[]) | a <- chars, b <- chars, c <- chars ]
        chars = ['a'..'z']

comma ',' _   = False
comma _   ',' = False
comma _   _   = True

stringToInt = foldl (\n m -> n*10 + m) 0 . map digitToInt

vowels str = sum . map count $ ['a','i','u','e','o','y']
  where count x = length $ filter (== x) str

main = do
  file <- readFile "p059_cipher.txt"
  let codes = map (chr . stringToInt) . trimDigits . removeComma $  groupBy comma file
  let cand  = maximumBy (comparing (vowels . decrypt codes)) $ candidates codes
  putStrLn . show $ decrypt codes cand
  putStrLn . show $ cand 
  putStrLn . show . sum . map ord $ decrypt codes cand

  where removeComma = filter (/= ",")
        trimDigits  = map (filter isDigit)

