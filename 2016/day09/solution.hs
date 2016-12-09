module Main where
import Data.List
import Data.List.Split
import Runner

dec rec [] = 0
dec rec ('(':t) = form rec + dec rec rest
  where mark = takeWhile (/=')') t
        [len, rep] = map read . splitOn "x" $ mark
        skip = length mark + 1
        inner = take len . drop skip $ t
        rest = drop (skip + len) t
        form False = len * rep
        form True = rep * dec rec inner
dec rec (_:t) = 1 + dec rec t

solve solver = show . sum . map (dec solver) . lines

main = runDay 9 [solve False, solve True]
