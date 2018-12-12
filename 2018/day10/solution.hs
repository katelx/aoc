import Data.List(unfoldr)

parse line = read $ "((" ++ p ++ "),(" ++ v ++ "))"
   where p = take 14 . drop 10 $ line
         v = take 6 . drop 36 $ line

move ((x, y), (a, b)) = ((x + a, y + b), (a, b))

decreasing_moves (prev_range, prev_state)
  | range < prev_range = Just (state, (range, state))
  | otherwise = Nothing
  where state = map move prev_state
        xs = map (fst . fst) state
        range = maximum xs - minimum xs

solution = unfoldr decreasing_moves . (,) (maxBound :: Int)

render ps = unlines (map rline yr)
  where xs = map fst ps
        ys = map snd ps
        xr = [minimum xs..maximum xs]
        yr = [minimum ys..maximum ys]
        rline y = map (pixel . (flip (,)) y) xr
        pixel p = if elem p ps then '#' else ' '

main = let input = map parse . lines <$> readFile "input" in do
  render . map fst . last . solution <$> input >>= putStr
  length . solution <$> input >>= print
