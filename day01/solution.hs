module Day01(solvers) where

val '(' = 1
val ')' = -1

firstNegative = length . takeWhile (>=0) . scanl (+) 0

solve solver = show . solver . map val

solvers = [solve sum, solve firstNegative]
