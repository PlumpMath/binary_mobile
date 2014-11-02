-- insects.hs

count a xs = length [x | x <-xs, x == a]
isOdd a xs = odd $ count a xs

odds xs = length [y | y <- "abc", isOdd y xs]
constant xs = null xs || count (xs!!0) xs == length xs
  
remainder xs
   | constant xs = length xs
   | od `elem` [0,3] = 2
   | od `elem` [1,2] = 1
   where od = odds xs
