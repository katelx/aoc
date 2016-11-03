module Main where
import Runner
import Data.List.Split(splitOn)

calcPaper [h, l, w] = 2 * sum a + minimum a where a = [h*l, h*w, l*w]

calcRibbon p@[h, l, w] = product p + 2 * minimum [h+l, h+w, l+w]

solve solver = show . sum . map (solver . map read . splitOn "x") . words

main = runDay 2 [solve calcPaper, solve calcRibbon]
