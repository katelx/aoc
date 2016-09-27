module Day10(solvers) where

import Data.List(group)

calc 0 ns = ns
calc n ns = calc (n-1) res
  where res = concat . map sizeprefix . group $ ns
        sizeprefix x = (show . length $ x) ++ [head x] 

solve solver = show . length . calc solver

solvers = [solve 40, solve 50]
