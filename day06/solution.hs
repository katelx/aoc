module Day06(solvers) where

import Data.List(foldl', foldl1')

type Point = (Int, Int)
data Switch = On | Off | Toggle

parseTuple :: String -> Point
parseTuple cords = read cords' where cords' = "(" ++ cords ++ ")"
parseSwitch switch tl br = (switch, parseTuple tl, parseTuple br)

parse ("toggle":tl:_:br:[]) = parseSwitch Toggle tl br
parse (_:"on":tl:_:br:[]) = parseSwitch On tl br
parse (_:"off":tl:_:br:[]) = parseSwitch Off tl br

matrix = [(x, y) | x <- [0..999], y <- [0..999]]

between (x1, y1) (x2, y2) (x, y) = x < x1 || x > x2 || y < y1 || y > y2 

switch p val (s, p1, p2)
  | between p1 p2 p = val
  | otherwise = case (val, s) of
      (_, On) -> 1
      (_, Off) -> 0
      (0, _) -> 1
      _ -> 0

bright p val (s, p1, p2)
  | between p1 p2 p = val
  | otherwise = case (val, s) of
      (val, On) -> val + 1
      (val, Off) -> max 0 (val - 1)
      (val, _) -> val + 2
      
summarize folder switches =  map (\cords -> foldl' (folder cords) 0 switches) matrix

solve folder = show . foldl1' (+) . summarize folder . map parse . map words . lines

solvers = [solve switch, solve bright]
