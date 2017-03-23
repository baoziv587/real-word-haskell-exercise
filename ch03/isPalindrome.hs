
isPalindrome  xs =  (first) == (reverse second)
                    where median  = round  ( fromIntegral (length xs)  / 2 )
                          tuble   = splitAt median xs 
                          first   = fst tuble
                          second  = snd tuble 
                           
                             

              