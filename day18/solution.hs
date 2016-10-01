module Day18(solvers) where

import qualified Data.Map as M

data Switch = On|Off|Perm 

parsel (n, xs) = map (\(m, x) -> ((n, m), x)) .  zip [1..] . map (\x -> if x == '#' then On else Off) $ xs

parse = M.fromList . concat . map parsel . zip [1..]

onoffval (Just On) = 1
onoffval (Just Perm) = 1
onoffval _ = 0

onoffvals d (x, y) = sum . map (\(a, b) -> onoffval $ M.lookup (x+a, y+b) d) $ [
  (1,1),(0,1),(-1,1),(1,0),(-1,0),(1,-1),(0,-1),(-1,-1)]

onoff d xy Perm = Perm
onoff d xy On | onoffvals d xy == 2 || onoffvals d xy == 3 = On
onoff d xy Off | onoffvals d xy == 3 = On
onoff _ _ _ = Off

maponoff 0 d = d
maponoff n d = maponoff (n-1) (M.mapWithKey (onoff d) d) 

cnt = M.foldl (\x y -> x + (case y of
                               Off -> 0
                               _ -> 1)) 0

setPerm d = foldl (\d' k -> M.insert k Perm d') d [(1,1),(1,100),(100,1),(100,100)]

solve solver = show . cnt . maponoff 100 . solver . parse . lines

solvers = [solve id, solve setPerm]
