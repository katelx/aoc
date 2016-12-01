module Main where
import Runner
import Data.List(group)

calc 0 ns = ns
calc n ns = calc (n-1) res
  where res = concat . map sizeprefix . group $ ns
        sizeprefix x = (show . length $ x) ++ [head x] 

solve solver = show . length . calc solver

main = runDay 10 [solve 40, solve 50]
