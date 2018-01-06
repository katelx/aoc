module Main where
import Runner

import Data.List.Split
import Data.List(transpose)
import qualified Data.Map as M

enhance 0 p m = p
enhance n p m = enhance (n-1) (chunksOf pc p >>= map concat . transpose . map ((M.!) m) . transpose . map (chunksOf pc)) m
  where pc = if even . length $ p then 2 else 3

solve it = show . length . filter (=='#') . concat . enhance it start . M.fromList . parse . lines
  where start = [".#.", "..#", "###"]
        parse ls = ls >>= pats . splitOn " => "
        pats [i, o] = zip (perms $ pat i) (repeat $ pat o)
        pat = splitOn "/"
        perms p = [p, reverse p] >>= (take 4 . iterate (reverse . transpose))

main = runDay 21 [solve 5, solve 18]
