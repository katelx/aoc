module Main where
import Runner

import Data.List.Split

vector "n" = [1, 0, 0]
vector "s" = [-1, 0, 0]
vector "ne" = [0, 1, 0]
vector "sw" = [0, -1, 0]
vector "nw" = [0, 0, 1]
vector "se" = [0, 0, -1]

dist = (\a -> sum a - minimum a) . map abs

solve solver = show . solver . map dist . scanl (zipWith (+)) [0, 0, 0] . map vector . splitOn ","

main = runDay 11 [solve last, solve maximum]
