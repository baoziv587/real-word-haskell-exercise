-- A function that should compute the length of list
-- mlen should has same defination as standard length

mlen :: [t] -> Int
mlen [] = 0 
mlen (x:xs) = count 1 xs
           where  count  n [] = n 
                  count  n (z:zs)  = count (n+1) zs


