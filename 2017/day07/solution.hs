module Main where
import Runner

import qualified Data.Map as M
import Data.List((\\),nub,sort)

parse :: [String] -> (String, (Int, [String]))
parse (n:w:"->":cs) = parse (n:w:cs)
parse (n:w:rs) = (n, (read w, clean rs))
  where clean [] = []
        clean [c] = [c]
        clean (c:cs) = init c:clean cs

root m = head $ M.keys m \\ M.foldl concat [] m
        where concat r (_, cs) = r ++ cs

children m n = snd $ M.findWithDefault (0, []) n m

value m = let val (v, cs) = (+ v) . sum . map (value m) $ cs in val . (M.!) m

correct m cs = if (null odd_cs) || (1 == (length $ nub (map (value m) odd_cs))) then valid-(sum (map (value m) odd_cs)) else correct m odd_cs
  where odd_n = head . filter (\c -> value m c == odd) $ cs
        (odd, valid) = pick . sort . map (value m) $ cs
        pick v@(a:b:_) = if a == b then (last v, a) else (a, b)
        odd_cs = children m odd_n

corrected m = show . correct m . children m $ root m
  
solve solver = solver . M.fromList . map (parse . words) . lines

main = runDay 7 [solve root, solve corrected]
