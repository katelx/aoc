module Main where
import Runner

import Data.List(dropWhile)

spin max_len solver len pos lst steps
  | len > max_len = solver pos lst
  | otherwise = spin max_len solver len' pos' lst' steps
  where pos' = succ . mod (pos + steps) $ len
        len' = succ len
        lst' = take pos' lst ++ [len] ++ drop pos' lst

next pos lst = lst !! (succ pos)

zero pos = head . tail . dropWhile (/= 0)

solve solver len = show . spin len solver 1 0 [0] . read

main = runDay 17 [solve next 2017, solve zero 50000000]
