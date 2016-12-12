module Main where
import Data.List
import Data.Maybe
import qualified Data.Map.Strict as M
import Runner

data Dest = Bot|Out deriving(Eq, Ord, Show)
data Op = In Int Int|Give Int (Dest, Int) (Dest, Int) deriving(Show)

parse [_, v, _, _, _, d] = In (read v) (read d)
parse [_, s, _, _, _, dl, dln, _, _, _, dh, dhn] = Give (read s) (dest dl, read dln) (dest dh, read dhn) where dest d = if d == "bot" then Bot else Out

bots m ops
  | M.null res = bots next ops
  | otherwise = snd . fst . head . M.toList $ res 
  where next = eval m ops
        res = M.filter (==[17, 61]) m

outs m ops
  | length res < 3 = outs next ops
  | otherwise = product . map (head . snd) $ res
  where next = eval m ops
        res = filter ((<3) . snd . fst) . filter ((==) Out . fst . fst) . M.toList $ m

eval m [] = m
eval m ((In v d):ops) = eval (proceed m (Bot, d) v) ops
eval m ((Give s (dl, dln) (dh, dhn)):ops)
  | M.notMember (Bot, s) m = eval m ops
  | otherwise = case fromJust . M.lookup (Bot, s) $ m of
                [lo, hi] -> eval (pass dh dhn hi . pass dl dln lo $ m) ops
                _ -> eval m ops
  where pass t d v m' = proceed m' (t, d) v

proceed m d v
  | M.notMember d m = M.insert d [v] m
  | otherwise = add . fromJust . M.lookup d $ m
  where add [x] | x /= v = M.insert d (sort [x, v]) m
        add _ = m
  

solve solver = show . solver M.empty . map (parse . words) . lines

main = runDay 10 [solve bots, solve outs]
