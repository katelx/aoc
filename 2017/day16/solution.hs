module Main where
import Runner

import Data.List.Split
import Data.List(takeWhile)

data M = S Int|E Int Int|P Char Char

parse c = case head c of
  's' -> S (read . tail $ c)
  'x' -> E (read p1) (read p2)
  'p' -> P (head p1) (head p2)
  where [p1, p2] = splitOn "/" . tail $ c

dance ps (S a) = let len = length ps in take len . drop (len - a) . cycle $ ps
dance ps (E a b) = dance ps (P (ps !! a) (ps !! b))
dance ps (P a b) = p ps
  where p (e:es)
          | e == a = b:p es
          | e == b = a:p es
          | otherwise = e:p es
        p [] = []

infdance ps ms = let dc = foldl dance ps ms in dc:infdance dc ms 

bildance ps ms = head . drop bil $ inf
  where period = (+ 1) . length . takeWhile (/= ps) $ inf
        bil = mod 1000000000 period - 1
        inf = infdance ps ms

solve solver = solver ['a'..'p'] . map parse . splitOn ","

main = runDay 16 [solve (foldl dance), solve bildance]
