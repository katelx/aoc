module Main where
import Runner

import qualified Data.Map as M

walk update pos steps m
  | pos < 0 || pos >= M.size m = steps
  | otherwise = walk update (pos + prev) (steps + 1) (M.insert pos new m)  
  where prev = (M.!) m pos
        new = update prev

solve update = show . walk update 0 0 . M.fromList . zip [0..] . map read . lines

inc_dec v = v + if v < 3 then 1 else -1

main = runDay 5 [solve succ, solve inc_dec]
