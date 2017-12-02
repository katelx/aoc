module Main where
import Runner

diffmm lst = maximum lst - minimum lst

diffdiv lst = head [div x y | x <- lst, y <- lst, x > y, mod x y == 0]

solve diff = show . sum . map (diff . map read . words) . lines

main = runDay 2 [solve diffmm, solve diffdiv]
