module Day09(solvers) where

import Data.Maybe(fromJust)
import Data.List(nub, permutations)
import qualified Data.Map as M

parse [a, "to", b, "=", d] = (a, b, read d :: Int)

double [] = []
double ((a, b, d):xs) = ((a, b), d):((b, a), d):double xs

getm abs = fromJust . M.lookup abs

dist m [a, b] = getm (a, b) m
dist m (a:b:s) = getm (a, b) m + dist m (b:s)

calc ds = map (dist m) p
  where abs = nub . map (fst . fst) $ ds
        p = permutations abs
        m = M.fromList ds  

solve solver = show . solver . calc . double . map parse . map words . lines

solvers = [solve minimum, solve maximum]
