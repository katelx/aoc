import qualified Data.Map as M

dirs = M.fromList [('N', \(x, y) -> (x, pred y)), ('E', \(x, y) -> (succ x, y)), ('S', \(x, y) -> (x, succ y)), ('W', \(x, y) -> (pred x, y))]

paths rs xyn@(xy, n) levels (c:cs) = case c of
  '$' -> M.elems rs
  '^' -> paths rs xyn levels cs
  '(' -> paths rs xyn (xyn:levels) cs
  '|' -> paths rs (head levels) levels cs
  ')' -> paths rs xyn (tail levels) cs
  _ -> paths rs' xyn' levels cs
  where xyn'@(xy', n') = ((M.!) dirs c $ xy, succ n) 
        rs' = if M.lookup xy' rs == Nothing then M.insert xy' n' rs else M.adjust (min n') xy' rs
  
main = let input = paths M.empty ((0, 0), 0) [] <$> readFile "input" in do
   maximum <$> input >>= print
   length . filter (>= 1000) <$> input >>= print
