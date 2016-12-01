module Main where
import Runner
  
val '(' = 1
val ')' = -1

firstNegative = length . takeWhile (>=0) . scanl (+) 0

solve solver = show . solver . map val

main = runDay 1 [solve sum, solve firstNegative]
