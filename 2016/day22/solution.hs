module Main where
import Runner
import Data.List
import Data.Char
import Data.Function

import Debug.Trace

data Node = Node { x :: Int, y :: Int, u :: Int, a :: Int, d :: [(Int, Int)] } deriving (Eq,Show)

xy n = (x n, y n)

parse [x,y,_,u,a,_] = Node { x = x, y = y, u = u, a = a, d = [(x, y)] }

digits = map read . filter (all isDigit) . groupBy (on (==) isDigit)

viable ns = [(na, nb) | na <- ns, nb <- ns, u na > 0, na /= nb, u na <= a nb]

step grid = [upd grid na nb | (na, nb) <- viable grid, path na nb]
  where upd (n:gs) na nb
          | xy n == xy na = n { u = 0, a = a na+u na, d = [] }:nxt
          | xy n == xy nb = n { u = u nb+u na, a = a nb-u na, d = sort (d na ++ d nb) }:nxt
          | otherwise = n:nxt
          where nxt = upd gs na nb
        upd [] _ _ = []
        path na nb = con x y || con y x
          where con f1 f2 = f1 na == f1 nb && abs (f2 na - f2 nb) == 1 

updated = filter touched
  where touched n = d n /= [xy n]

steps cnt grids | trace (show cnt ++ ": " ++ show (length grids) ++ "\n" ++ unlines (map (unlines . map show . updated) grids)) False = undefined
steps cnt grids
  | any (elem goal . first) grids = cnt
  | otherwise = steps (cnt+1) (nubBy (on (==) updated) grids >>= step)
  where goal = xy . last . filter ((==0) . y) . head $ grids
        first grid = d . head . filter ((==(0, 0)) . xy) $ grid

solve solver = show . solver . map (parse . digits) . drop 2 . lines

main = runDay 22 [solve (length . viable), solve (steps 0 . return)]