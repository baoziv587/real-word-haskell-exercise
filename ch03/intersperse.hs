-- file: ch03/Intersperse.hs
intersperse :: a -> [[a]] -> [a]


intersperse connector xs
          | null xs          = []
          | (length xs) ==1  = head xs 
          | otherwise        = head xs ++ [connector] ++ (intersperse connector (tail xs ))
