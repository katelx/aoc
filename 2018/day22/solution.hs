import qualified Data.Map as M
import qualified Data.Set as S

riskLevels offset (depth, (tx, ty)) = M.map (flip mod 3) erosionLevels
  where erosionLevels = M.fromList [(xy, erosionLevel xy) | xy <- (,) <$> [0..tx + offset] <*> [0..ty + offset]]
        erosionLevel xy = mod (geologicIndex xy + depth) 20183
        geologicIndex (x, y)
          | y == 0 = x * 16807
          | x == 0 = y * 48271
          | x == tx && y == ty = 0
          | otherwise = (M.!) erosionLevels (pred x, y) * (M.!) erosionLevels (x, pred y)

findPath tgt@(xy@(x, y), t) idx prev cur = case M.lookup xyt prev of
  Just d' -> if d' <= d then skip else next
  _ -> next
  where skip = findPath tgt idx prev cur'
        next = if tgt == xyt then d else findPath tgt idx prev' cur''
        dxyt@(d, xyt) = S.findMin cur
        prev' = M.insert xyt d prev
        cur' = S.delete dxyt cur
        cur'' = S.union cur' . S.fromList $ moves idx dxyt

moves idx (d, ((x, y), t)) = [(d + 7, ((x, y), t')) | t' <- [0..2], t' /= t, t' /= (M.!) idx (x, y)] ++ [(succ d, (xy, t)) | let ds = [-1..1], i <- ds, j <- ds, abs i /= abs j, let xy = (x + i, y + j), let t' = M.lookup xy idx, t' /= Nothing, t' /= Just t]
  
shortestPath dxy@(_, xy) = let idx = riskLevels 64 dxy in findPath (xy, 1) idx M.empty (S.singleton (0, ((0, 0), 1)))

parse [_, depth, _, target] = (read depth, read . ("(" ++) . (++ ")") $ target)

main = let input = parse . words <$> readFile "input" in do
  sum . M.elems . riskLevels 0 <$> input >>= print
  shortestPath <$> input >>= print
