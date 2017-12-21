module Main where
import Runner

import Data.Char(isAlpha)
import Data.List(elemIndices)

uu = 0
dd = 1
ll = 2
rr = 3

steps (-1, -1, _) m = '|':steps n m
  where n = (head . elemIndices '|' . head $ m, 1, dd)
steps (x, y, d) m
  | c == ' ' = []
  | otherwise = c:steps (nx, ny, nd) m
  where c = m !! y !! x
        tx = if (m !! y !! (x+1)) == '-' then (x+1, y, rr) else (x-1, y, ll)
        ty = if (m !! (y+1) !! x) == '|' then (x, y+1, dd) else (x, y-1, uu)
        (nx, ny, nd) = [[tx, tx, ty, ty] !! d, [(x, y-1, d), (x, y+1, d), (x-1, y, d), (x+1, y, d)] !! d] !! (if c == '+' then 0 else 1)

letters = filter isAlpha

len = show . length

solve solver = solver . steps (-1, -1, dd) . lines

main = runDay 19 [solve letters, solve len]
