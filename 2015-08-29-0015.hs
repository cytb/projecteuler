
-- sol by math

fact n = factr n 1
  where factr 0 m = m
        factr n m = factr (n-1) (n*m)

main = putStrLn . show $ (fact 40) `div` ((fact 20)^2)## A New Post

Enter text in [Markdown](http://daringfireball.net/projects/markdown/). Use the toolbar above, or click the **?** button for formatting help.
