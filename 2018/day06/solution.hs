import Data.List(sort, groupBy)
import Data.Function(on)

points ps = (,) <$> [xa..xb] <*> [ya..yb]
  where xs = map fst ps
        ys = map snd ps
        (xa, xb) = (minimum xs, maximum xs)
        (ya, yb) = (minimum ys, maximum ys)

region ps = last . init . sort . map length . groupBy (on (==) snd) . sort . (>>= closest ps) . points $ ps

dist (x, y) (a, b) = abs (a - x) + abs (b - y)

closest ps (x, y) = valid . sort . map area $ ps
  where area m = (dist (x, y) m, m)
        valid ((a, m):(b, _):_) | a == b = [] | otherwise = [m]

center ps = length . filter (< 10000) . map (sum . (flip map) ps . dist) . points $ ps

main = let input = map (read . ("(" ++) . (++ ")")) . lines <$> readFile "input" in do
  region <$> input >>= print
  center <$> input >>= print
