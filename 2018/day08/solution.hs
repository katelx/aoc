
data N = N [N] [Int]

build 0 _ = []
build n (c:m:ds) = (2 + s + m, N ns ms):build (pred n) (drop (s + m) ds)
  where cs = build c ds
        s = sum (map fst cs)
        ns = map snd cs
        ms = take m . drop s $ ds

msum (N cs ms) = sum ms + sum (map msum cs)

rsum (N [] ms) = sum ms
rsum (N cs ms) = sum (map rsum' ms)
  where rsum' m | m > 0 && m <= length cs = rsum (cs !! pred m) | otherwise = 0

main = let input = snd . head . build 1 . map read . words <$> readFile "input" in do
  msum <$> input >>= print
  rsum <$> input >>= print
