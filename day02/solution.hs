module Main where
import Runner
import Data.List(groupBy)

parse = map read . filter (/= "x") . groupBy (\x y -> x /= 'x' && y /= 'x')

calcPaper [h, l, w] = (+ minimum ar) . sum $ map (*2) ar where ar = [h*l, h*w, l*w]

calcRibbon [h, l, w] = minimum par * 2 + h * l * w where par = [h+l, h+w, l+w]

solve solver = show . sum . map solver . map parse . words

main = runDay 2 [solve calcPaper, solve calcRibbon]
