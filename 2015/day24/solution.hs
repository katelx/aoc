module Main where
import Runner
import Data.List(tails)

comb 0 lst = [[]]
comb n lst = [x:cx | x:xs <- tails lst, cx <- comb (n-1) xs]

calc k d lst = if null res then calc (k+1) d lst else minimum res
  where res = map product . filter ((==mx) . sum) . comb k $ lst
        mx = div (sum lst) d
  
solve cnt = show . calc 1 cnt . map read . words

main = runDay 24 [solve 3, solve 4]
