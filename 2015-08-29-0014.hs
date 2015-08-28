import Data.List(maximumBy)
import Data.Function(on)
import Data.Word

collatz :: Word32 -> (Word32,Word32)
collatz n = collatzr n 1 n

collatzr n m r
  | r == 1    = (n,m)
  | even r    = collatzr n (m+1) (r `div` 2)
  | otherwise = collatzr n (m+1) (3*r + 1)

main = putStrLn . show . maximumBy (compare `on` snd) . map collatz $ [1..1000000]## A New Post

Enter text in [Markdown](http://daringfireball.net/projects/markdown/). Use the toolbar above, or click the **?** button for formatting help.
