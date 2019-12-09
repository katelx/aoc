import qualified Data.Map as M
import qualified Data.Text as T

parse = let p q = replicate (read . tail $ q) (head q) in concat . map (p . T.unpack) . T.splitOn (T.pack ",") . T.pack

points = M.fromList . tail . flip zip [0..] . scanl p (0, 0)
  where p (x, y) d = case d of
          'R' -> (x+1, y)
          'U' -> (x, y+1)
          'L' -> (x-1, y)
          'D' -> (x, y-1)

main = let f = minimum . M.elems in do
  [a, b] <- map (points . parse) . lines <$> readFile "input"
  print (f $ M.intersectionWithKey (\(x, y) _ _ -> abs x + abs y) a b)
  print (f $ M.intersectionWith (+) a b)
