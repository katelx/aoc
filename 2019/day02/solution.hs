import qualified Data.Map as M
import qualified Data.Text as T

parse = M.fromList . zip [0..] . map (read . T.unpack) . T.splitOn (T.pack ",") . T.pack

run p m = case o of
  1 -> run j (op (+))
  2 -> run j (op (*))
  99 -> (M.!) m 0
  where o = (M.!) m p
        j = p + 4
        op f = M.insert ((M.!) m (p + 3)) (f (v 1) (v 2)) m
        v c = (M.!) m ((M.!) m (p + c))

f n v = run 0 . M.insert 2 v . M.insert 1 n

s = f 12 2

p m = head [n * 100 + v | n <- [0..99], v <- [0..99], f n v m == 19690720]
 
main = let c v = v . parse <$> readFile "input" >>= print in c s >> c p
