-- give [a,b,c] return [a,b,c,c,b,a]


palindrome :: [a] -> [a]
palindrome  xs  = xs ++ reverse xs

