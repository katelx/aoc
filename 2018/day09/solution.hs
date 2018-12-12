import qualified Data.IntMap as M
import Data.List(foldl')

parse mul ws = (zip [1..s] (cycle [1..p]), M.fromList (zip [1..p] (repeat 0)))
  where (p, s) = (read . head $ ws, (* mul) . read . last . init $ ws)

cir_init = (0, M.fromList [(0, (0, 0))])

clock i cir@(p, ms)
  | i > 0 = clock (pred i) (next, ms)
  | i < 0 = clock (succ i) (prev, ms)
  | otherwise = cir
  where (prev, next) = (M.!) ms p

cir_ins m cir = (m, M.adjust acur cur . M.adjust anext next . M.insert m (cur, next) $ ms)
  where (cur, ms) = clock 1 cir
        (_, next) = (M.!) ms cur
        acur (c, _) = (c, m)
        anext (_, c) = (m, c)

cir_rem cir = (cur, (next, M.adjust aprev prev . M.adjust anext next . M.delete cur $ ms))
  where (cur, ms) = clock (-7) cir
        (prev, next) = (M.!) ms cur
        anext (_, c) = (prev, c)
        aprev (c, _) = (c, next)

play (cir, ps) (m, p)
  | mod m 23 == 0 = let (n, cir') = cir_rem cir in (cir', M.adjust ((+ m) . (+ n)) p ps)
  | otherwise = (cir_ins m cir, ps)

solve mul ws = maximum . M.elems . snd . foldl' play (cir_init, ps) $ ms
  where (ms, ps) = parse mul ws

main = let input = words <$> readFile "input" in do
  solve 1 <$> input >>= print
  solve 100 <$> input >>= print
