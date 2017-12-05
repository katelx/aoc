module Main where
import Runner
import Data.Array.IO
import Data.Array.Base

walk :: (Int -> Int) -> Int -> Int -> Int -> IOUArray Int Int -> IO Int
walk update len pos steps arr
  | pos < 0 || pos >= len = return steps
  | otherwise = do
      prev <- unsafeRead arr pos
      unsafeWrite arr pos (update prev)
      walk update len (pos + prev) (steps + 1) arr

solve update input = newListArray (0, len) steps >>= walk update len 0 0 >>= return . show
  where steps = map read . lines $ input
        len = length steps

inc = (+ 1)

inc_dec v = v + if v < 3 then 1 else -1

main = runDayM 5 [solve inc, solve inc_dec]
