module Day13(solvers) where

import Data.Maybe(fromJust)
import Data.List(nub, permutations)
import qualified Data.Map as M

parse [a, _, "gain", h, _, _, _, _, _, _, b] = ((a, init b), read h :: Int)
parse [a, _, "lose", h, _, _, _, _, _, _, b] = ((a, init b), -1 * read h :: Int)

getm abs = fromJust . M.lookup abs
getm2 m (a, b) = getm (a, b) m + getm (b, a) m

doubles [a, b] = [(a, b)]
doubles (a:b:s) = (a, b):doubles (b:s)

hap m hs = sum $ map (getm2 m) ((head hs, last hs):doubles hs)

calc hs = map (hap m) p
  where p = permutations . absNub $ hs
        m = M.fromList hs

absNub = nub . map (fst . fst)

addZero hs = hs ++ map f u ++ map s u
  where u = absNub hs
        f = \x -> ((x, "Zero"), 0)
        s = \x -> (("Zero", x), 0)

solve solver = show . maximum . calc . solver . map parse . map words . lines

solvers = [solve id, solve addZero]
