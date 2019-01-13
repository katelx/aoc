import qualified Data.Map as M

cyc prev new@((i, a):ias)
  | M.member a prev = head . drop (mod (1000000000 - i) (i - pi)) . map snd $ new
  | otherwise = cyc (M.insert a i prev) (ias)
  where pi = (M.!) prev a

value a = cnt '|' * cnt '#'
  where cnt c = length . filter (== c) . M.elems $ a

update a = M.mapWithKey update' a
  where update' xy c = let adj = adjacent a xy in new c adj
        new '.' adj | atLeast 3 '|' adj = '|'
        new '|' adj | atLeast 3 '#' adj = '#'
        new '#' adj | not (atLeast 1 '#' adj && atLeast 1 '|' adj) = '.'
        new c _ = c
        atLeast n c = (>= n) . length . filter (== c)

adjacent a (x, y) = [(M.!) a (x + i, y + j) | i <- [-1..1], j <- [-1..1], i /= 0 || j /= 0, M.member (x + i, y + j) a]

parse = M.fromList . concatMap parse' . zip [0..] . lines
  where parse' (y, ln) = [((x, y), c) | (x, c) <- zip [0..] ln]

main = let input = iterate update . parse <$> readFile "input" in do
  value . head . drop 10 <$> input >>= print
  value . cyc M.empty . zip [0..] <$> input >>= print
