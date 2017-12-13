module Main where
import Runner


parse [d, r] = (read (init d), read r)

pos (t, r) = mod t (r * 2 - 2)

add n (t, r) = (t + n, r)

scan = sum . map (uncurry (*)) . filter ((== 0) . pos)

delay n t | all ((/= 0) . pos . add n) t = n | otherwise = delay (n + 1) t

solve solver = show . solver . map (parse . words) . lines
  
main = runDay 13 [solve scan, solve (delay 0)]
