module Main where
import Runner
import Data.Char

parse = pair . map read . filter (all isDigit) . words . init
  where pair [m, p] = (m, p)

delay = map add . zip [1..]
  where add (d, (m, p)) = (m, mod (p + d) m)

time now xs | all ((==0) . snd) xs = now
time now xs = time (now + 1) (map inc xs)
  where inc (m, p) = (m, mod (p + 1) m)

solve solver = show . time 0 . delay . solver .  map parse . lines

main = runDay 15 [solve id, solve (++ [(11, 0)])]
