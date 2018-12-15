import qualified Data.Map as M
import Data.List(unfoldr,foldl')

mcarts = M.fromList [('^', (0, 0)), ('>', (1, 0)), ('v', (2, 0)), ('<', (3, 0))]

mroads = M.fromList $ [
  ('+', \(d, n) -> ([[3, 0, 1, 2], [0..], [1, 2, 3, 0], [0..]] !! mod n 3 !! d, succ n)),
  ('/', \(d, n) -> ([1, 0, 3, 2] !! d, n)),
  ('\\', \(d, n) -> ([3, 2, 1, 0] !! d, n)),
  ('-', id), ('|', id)] ++ map (flip (,) id) (M.keys mcarts)

parse m = M.fromList . concatMap (\(y, ln) -> [((x, y), (M.!) m c) | (x, c) <- zip [0..] ln, M.member c m]) . zip [0..]

next roads ((x, y), (dir, _)) = let xy = (x + [0, 1, 0, -1] !! dir, y + [-1, 0, 1, 0] !! dir) in (xy, (M.!) roads xy)

crash roads op@(left, crashed, survived) cart
  | not (mpos left) = op
  | mpos' left' || mpos' survived = (left'', crashed', survived')
  | otherwise = (left', crashed, M.insert pos' cart' survived)
  where pos = fst cart
        mpos = M.member pos
        (pos', move) = next roads cart
        mpos' = M.member pos'
        cart' = move . snd $ cart
        left' = M.delete pos left
        left'' = M.delete pos' left'
        crashed' = crashed ++ [pos']
        survived' = M.delete pos' survived

tick (roads, carts)
  | M.null carts = Nothing
  | M.size carts == 1 = Just (M.keys carts, (roads, M.empty)) 
  | otherwise = Just (crashes, (roads, survived))
  where (_, crashes, survived) = foldl' (crash roads) (carts, [], M.empty) (M.toAscList carts)

main = let input = lines <$> readFile "input" in do
  cs <- parse mcarts <$> input
  rs <- parse mroads <$> input
  let crashes = concat $ unfoldr tick (rs, cs)
  print $ head crashes
  print $ last crashes
