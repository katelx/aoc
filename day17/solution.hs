module Main where
import Runner
import Data.List(delete)

comb xs k = comb' (length xs) k xs
  where comb' n k' l@(y:ys)
          | k' == 0   = [[]]
          | k' >= n   = [l]
          | null l    = []
          | otherwise = map (y :) (comb' (n - 1) (k' - 1) ys) ++ comb' (n - 1) k' ys 

calc ns = filter (\x -> sum x == 150) $ [1..(length ns - 1)] >>= comb ns

mins ns = length . filter (\x -> length x == minl) $ ns
  where minl = minimum . map length $ ns

solve solver = show . solver . calc . map read . lines

main = runDay 17 [solve length, solve mins]
