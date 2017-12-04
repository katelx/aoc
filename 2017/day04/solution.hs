module Main where
import Runner
import Data.List(nub,sort)
import Data.Function(on)


solve m = show . length . filter (\w -> on (==) length w . nub $ w) . map (map m . words) . lines

main = runDay 4 [solve id, solve sort]
