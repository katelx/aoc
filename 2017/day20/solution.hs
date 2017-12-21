module Main where
import Runner

import Data.List.Split
import Data.List(minimumBy,sort,groupBy,zipWith)
import Data.Function(on)

parse = chunksOf 3 . map read . wordsBy (flip elem "pva=<>, ")

closest = fst . minimumBy (on compare snd) . zip [0..] . map (sum . map abs . head . drop 2)

next [p, v, a] = [s av p, av, a]
  where s = zipWith (+)
        av = s a v

collided a vs
  | a == 0 = length vs
  | otherwise = collided (a-1) $ filter (not . flip elem rem) nvs
  where rem = concat . filter ((> 1) . length) . groupBy (on (==) head) . sort $ nvs
        nvs = map next vs

solve solver = show . solver . map parse . lines

main = runDay 20 [solve closest, solve (collided 64)]
