
-- ch2  exercises 2 
lastButOne (xs) = if length xs == 2 then head xs else  lastButOne (tail xs)