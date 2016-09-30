module Day15(solvers) where

import Data.Char(isDigit)
import Data.List(zipWith, transpose)

parse =  map read . words . filter (\x -> isDigit x || x == '-' || x == ' ')

genp xss = [x:xs | x <- [0..100], xs <- xss, sum (x:xs) <= 100]
genps xss 0 = xss
genps xss n = genps (genp xss) (n-1)

score calpred ing qty = if all (>0) isums && calpred lsums then product isums else 0 
  where zipper = \i -> sum $ zipWith (*) i qty
        sums = map zipper ing
        isums = init sums
        lsums = last sums

calc calpred ing = maximum . map (score calpred ting) $ qty
  where qty = filter (\x -> sum x == 100) $ genps [[]] (length ing)
        ting = transpose ing

solve solver = show . calc solver . map parse . lines

solvers = [solve (\_ -> True), solve (==500)]
