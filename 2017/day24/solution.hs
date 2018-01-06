module Main where
import Runner

import Data.List.Split
import Data.List(delete, maximumBy)
import Data.Function(on)


parse c = let [c1, c2] = map read . splitOn "/" $ c in (c1, c2)

chain ag@(al, as) prv by ps = maximumBy (on compare by) . (ag :) . map next . filter hasn $ ps
  where next c = chain (succ al, as + uncurry (+) c) (if prv == fst c then snd c else fst c) by (delete c ps)
        hasn (a, b) = a == prv || b == prv

solve solver = show . snd . chain (1, 0) 0 solver . map parse . lines

main = runDay 24 [solve snd, solve id]
