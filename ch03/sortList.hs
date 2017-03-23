import Data.List

cp a b =  compare  (length b)  (length a)
sort1  xs = sortBy cp  xs 