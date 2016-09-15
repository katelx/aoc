module Day03(solvers) where

import Data.List(nub,partition)

val '^' = (0, 1)
val '>' = (1, 0)
val 'v' = (0, -1)
val '<' = (-1, 0)

tupAdd (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

calcTrail = scanl tupAdd (0, 0)

splitEvenOdd = map (map snd) . (\t -> [fst t, snd t]) . partition (even . fst) . zip [1..]

calcTrails = concat . map calcTrail . splitEvenOdd

solve solver = show . length . nub . solver . map val 

solvers = [solve calcTrail, solve calcTrails]
