module Main where
import Runner
import Data.List(transpose)

chkt [] = []
chkt (a:b:c:ts) = (a + b > c && a + c > b && b + c > a):chkt ts

solve solver = show . length . filter id . chkt . concat . solver . map (map read . words) . lines

main = runDay 3 [solve id, solve transpose]
