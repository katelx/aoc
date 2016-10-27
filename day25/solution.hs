module Main where
import Runner
import Data.Char(isDigit)

parse :: String -> (Int, Int)
parse txt = (x, y)
  where [y, x] = map read . filter (all isDigit) . map init . words $ txt

calc = (`rem` 33554393) . (* 252533)

find cur val t | cur == t = val
find (x, 1) val t = find (1, x + 1) (calc val) t
find (x, y) val t = find (x + 1, y - 1) (calc val) t

solve = show . find (1, 1) 20151125 . parse

main = runDay 25 [solve]
