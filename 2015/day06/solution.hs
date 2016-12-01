module Main where
import Runner
import Data.Array.IO
import Data.Array.Base
import Control.Monad

data Switch = On | Off | Toggle

parseTuple cords = read cords' where cords' = "(" ++ cords ++ ")"
parseSwitch switch tl br = (switch, (parseTuple tl, parseTuple br))

parse :: [String] -> (Switch, ((Int, Int), (Int, Int)))
parse ("toggle":tl:_:br:[]) = parseSwitch Toggle tl br
parse (_:"on":tl:_:br:[]) = parseSwitch On tl br
parse (_:"off":tl:_:br:[]) = parseSwitch Off tl br

switch (s, p) = (case s of
      On -> const 1
      Off -> const 0
      _ -> \v -> if v == 1 then 0 else 1, p)

bright (s, p) = (case s of
      On -> (+1)
      Off-> \v -> if v == 0 then 0 else v - 1
      _ -> (+2), p)

updatey arr f x y1 y2
  | y1 > y2 = return arr
  | otherwise = unsafeRead arr i >>= unsafeWrite arr i . f >> updatey arr f x (y1+1) y2
  where i = x + y1

updatex arr f x1 x2 y1 y2
  | x1 > x2 = return arr
  | otherwise = updatey arr f (x1*1000) y1 y2 >> updatex arr f (x1+1) x2 y1 y2

update arr (f, ((x1, y1), (x2, y2))) = updatex arr f x1 x2 y1 y2 >> return arr

eval_funs switches arr = mapM (update arr) switches >> return arr

eval_sum :: Int -> Int -> IOUArray Int Int -> IO Int
eval_sum 1000000 s arr = return s
eval_sum i s arr = unsafeRead arr i >>= \v -> eval_sum (i+1) (s+v) arr

solve trans input = newArray (0, 999999) 0 >>= eval_funs switches >>= eval_sum 0 0 >>= return . show
  where switches = map (trans . parse . words) . lines $ input
        
main = runDayM 6 [solve switch, solve bright]
