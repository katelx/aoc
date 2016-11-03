module Main where
import Runner
import Data.List(nub,partition,zipWith)

val '^' = [0, 1]
val '>' = [1, 0]
val 'v' = [0, -1]
val '<' = [-1, 0]

calcTrail = scanl (zipWith (+)) [0, 0]

calcTrails = trails . partition (even . fst) . zip [1..]
  where trail = calcTrail . map snd
        trails (x, y) = trail x ++ trail y

solve solver = show . length . nub . solver . map val 

main = runDay 3 [solve calcTrail, solve calcTrails]
