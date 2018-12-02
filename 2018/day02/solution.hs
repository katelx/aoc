import Data.List(group, sort)
import Data.Tuple(uncurry)

chksum xs = cnt 2 xs * cnt 3 xs
  where cnt len = length . filter (any ((== len) . length) . group . sort)

diff xs = head [map fst $ chk (==) a b | a <- xs, b <- xs, (== 1) . length $ chk (/=) a b]
  where chk cmp a b = filter (uncurry cmp) $ zip a b

main = let input = words <$> readFile "input" in do
  chksum <$> input >>= print
  diff <$> input >>= print
