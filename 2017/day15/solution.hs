module Main where
import Runner

import Data.Function(on)

gen [a, b] = [g 16807 a, g 48271 b]
  where g m s = let c = n m s in c:g m c
        n m = flip mod 2147483647 . (* m) 

matching [a:as, b:bs] = on (==) (flip mod 65536) a b:matching [as, bs]

moded [as, bs] = [m 4 as, m 8 bs]
  where m d = filter ((== 0) . flip mod d)

solve pred pairs = show . length . filter id . take pairs . matching . pred . gen . map (read . last . words) . lines

main = runDay 15 [solve id 40000000, solve moded 5000000]
